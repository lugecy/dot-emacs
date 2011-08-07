require 'yaml/store'

def git_update
  sh %{git fetch origin}
  sh %{git checkout master}
  sh %{git merge origin/master}
end

yaml_fname = File.dirname(__FILE__) + "/git-repo.yaml"
GIT_REPO = YAML::Store.new(yaml_fname)

GIT_REPO.transaction(true) do |db|
  keys = db.roots
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
      cd dir
      git_update
      new_head = `git rev-parse master`.chomp
      GIT_REPO.transaction do |db|
        db[k][2] = new_head
      end
    end
  end
end
