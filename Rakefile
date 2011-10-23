require 'yaml/store'

def git_update
  sh %{git fetch origin}
  sh %{git checkout master}
  sh %{git merge origin/master}
end

yaml_fname = File.dirname(__FILE__) + "/git-repo.yaml"
GIT_REPO = YAML::Store.new(yaml_fname)

GIT_REPO.transaction(true) do |db|
  keys = db.roots.reject{|key| key == "checked" }
  keys.each do |k|
    v = db[k]
    dir, url, commit = *v
    task "setup-#{k}" do
      cd dir
      if FileTest.exist?(".git")
        puts "already init #{k}"
      else
        sh %{git init}
        sh %{git remote add origin #{url}}
        sh %{git fetch origin}
        sh %{git branch master #{commit}}
        sh %{git branch --set-upstream master origin/master}
        sh %{git reset master}
      end
    end

    task "update-#{k}" do
      now_head = nil
      GIT_REPO.transaction(true) do |db|
        now_head = db[k][2]
      end
      cd dir
      git_update
      new_head = `git rev-parse master`.chomp
      unless now_head == new_head
        GIT_REPO.transaction do |db|
          db[k][2] = new_head
        end
      end
    end
  end
  task "liner-update" do
    current_dir = pwd
    checked_list = GIT_REPO.transaction(true){|db| db["checked"] } || []
    keys.each do |k|
      next if checked_list.index(k)
      puts "update #{k}..."
      now_head = GIT_REPO.transaction(true){|db| db[k][2] }
      update_flg = Rake::Task["update-#{k}"].invoke
      new_head = GIT_REPO.transaction(true){|db| db[k][2] }
      checked_list << k
      unless now_head == new_head
        puts "found update #{k}"
        break
      end
      cd current_dir
      puts
    end
    GIT_REPO.transaction do |db|
      db["checked"] = (keys.length == checked_list.length ? [] : checked_list)
    end
  end
end
