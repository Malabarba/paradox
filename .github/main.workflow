workflow "Test and deploy to Heroku" {
 on = "push"
 resolves = ["docker.build", "docker.test"]
}

action "docker.build" {
 uses = "actions/docker/cli@master"
 args = "build -f Dockerfile -t ci-$GITHUB_SHA:latest ."
}

action "docker.test" {
 uses = "actions/docker/cli@master"
 args = "run ci-$GITHUB_SHA:latest"
 needs = ["docker.build"]
}
