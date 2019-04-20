workflow "Test and deploy to Heroku" {
  resolves = [
    "docker.test",
    "docker.build",
    "filter.master",
  ]
  on = "push"
}

action "docker.build" {
  uses = "actions/docker/cli@master"
  args = "build -f Dockerfile -t ci-$GITHUB_SHA:latest ."
  needs = ["filter.master"]
}

action "docker.test" {
  uses = "actions/docker/cli@master"
  args = "run ci-$GITHUB_SHA:latest"
  needs = ["docker.build"]
}

action "filter.master" {
  uses = "actions/bin/filter@3c0b4f0e63ea54ea5df2914b4fabf383368cd0da"
  args = "branch master"
}
