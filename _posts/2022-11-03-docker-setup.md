---
title: "Docker setup"
date: 2022-11-03 09:00:00 +0200
categories: tool
tags: docker
---

In one task I am currently assigned to, I need to work with multiple docker containers.
In this post, I will document the installation of `docker`, the basic usage, and if possible explaining the communication between multiple containers.

### Installation

There are 2 installation options listed on Docker's websites: 1. Docker-desktop and 2. Docker engine. 
As I understand, Docker-desktop is a bundle of Docker engine and other useful tools, via GUI. 

Most importantly, the desktop provides a linux VM wrap for above tools to make sure by using Docker-desktop, one can run the same application across different OSes.
Docker-desktop is a necessity for Mac and Windows but not necessary for Linux, because on Linux one can directly start the engine on the Linux kernel.

Also, I couldn't install Desktop on Ubuntu 20.04 (in VM), mostly perhaps it is only supported for latest Ubuntu 22.04 (21.04).

Therefore, I followed [this page](https://docs.docker.com/engine/install/ubuntu/#install-using-the-repository) to install Docker engine.
But I haven't adjusted the user permissions, so I need `sudo` for all docker commands.


### Terms

Source: [get-started page](https://docs.docker.com/get-started/) and [Docker overview](https://docs.docker.com/get-started/overview/).

- container: 

    It is a sandboxed process which is isolated from all other processes on the host machine. 
    It is also isolated form other containers and runs its own application, binaries and configuration.
    More formally, it is a runnable instance of a container image, which one can use CLI to operate it.
    
- docker/container image:

    It is a read-only template with instructions for creating a docker container.
    Each container uses an isolated filesystem, which is provided by the image.
    It also provides default command to start the container, and environment variables.
    One can build own images by creating a `Dockerfile` for defining the steps needed to create the image and run it, each instruction in the `Dockerfile` creates a layer in the image.
    
- docker engine:

    It is an open source containerization technology which acts as a client-server application.
    It contains a server with a long-running daemon process, APIs specifying how programs can communication to the docker daemon, and a CLI client.
    
- docker daemon:

    It is a background service that runs between the containers and the Linux kernel, it listens for Docker API requests and perform management for containers and network.
   When using APIs such as `docker run`, docker client sends the command to the daemon. 
    
- docker registry and docker hub:

    It stores docker image.
    Docker hub is a public registry, and by default docker will look for images on Docker hub.
    One can also run private registry.
    When using docker API `docker pull` and `docker run`, the required images are pulled from configured registry.
    When using `docker push`, the image is pushed to the registry.
    
### Usage

Source: [get-started page](https://docs.docker.com/get-started/) 

#### Ex 1: an example

When running command `docker run -i -t ubuntu /bin/bash`, the following things happen:

1. if the ubuntu image is not there locally, Docker first runs `docker pull ubuntu`

2. creates a new container with `docker container create`

3. allocates a read-write filesystem to the container as its final layer

4. creates a network interface to connect the container to the default network, including assigning IP address. By default containers can connect to external networks using the host machine's network connection
    
5. starts the container and executes `/bin/bash` and interactively (`-i`) attach it to the terminal (`-t`)

6. when using `exit` to terminate `/bin/bash` command, the container stops but is not removed

#### Ex 2: basic commands

An exmaple `Dockerfile` looks like the following:

```dockerfile
# syntax=docker/dockerfile:1

# FROM <base-image>:<code-version>
# initialize a new build stage and sets the base image
FROM node:12-alpine

# RUN <shell-cmd>
RUN apk add --no-cache python2 g++ make

# WORK <path/to/workdir>
# the pwd when entering the container
WORKDIR /app

COPY . .

RUN yarn install --production

# CMD ["executable" "param1" "param2"] 
# provides default for executing a container
CMD ["node", "src/index.js"]

# EXPOSE <port> informs Docker the port number the container is listening
EXPOSE 3000
```

Then in the same path where the dockerfile is located, one can build the container image with `docker build -t getting-started .`(note that it has **not** created any container yet), where `-t` the human-readable container name `getting-started` is used for this image. The `.` tells Docker it should look for Dockerfile in the current directory.

Then one can run the application in the container with `docker run -dp 3000:3000 getting-started` (now a container is created with the `getting-started` image), where `-d` runs the container in the background (i.e., detached mode), and `-p` specifies the port mapping between host's 3000 to the container's port 3000.

Then one can open the application in host's web browser `localhost:3000` (it's a web app).

Whenever some updates are performed in the app's source code, one can rebuild the image (i.e.,g `docker build`), stop and remove the old container (as only one container can listen to 3000) and start a new container with the new image (i.e., `docker run`).

To stop a container, one first need the container id with `docker ps`, then run `docker stop <container-id>` to stop and `docker rm <container-id>` to remove.
One can also run `docker rm -f <container-id>` to stop and remove the container in one line.

One can then push the image to the public docker hub.
To do this, an account is first required on the docker hub, then create a new repository on the website with the name `getting-started`.

Next, login to the account via CLI `docker login -u <user-name>`, and create an alias with `docker tag getting-started <user-name>/getting-started`, then in `docker image ls` there will be 2 same images with different names.
Push the image with `docker push <user-name>/getting-started`, by default the pushed image will have the version tag `latest`.

Finally, on another instance, one can pull and run this image with `docker run -dp 3000:3000 <user-name>/getting-started`.

#### Ex 3: named-volume

Even when two containers can built from the same image, they have separate filesystems, hence any file change made in one container won't be seen in another container.

To verify this, first pull and run an ubuntu image in a new container with `docker run -d ubuntu bash -c "shuf -i 1-10000 -n 1 -o /data.txt && tail -f /dev/null`.
This command starts an ubuntu image and execute 2 commands in the bash in the background, command 1 writes a random number to `/data.txt` and command 2 watches a file to keep the container running.

One can then check the file with `docker exec <container-id> cat /data.txt`, note that `exec` executes a command in a running container, while `run` will create a new container.

Next, one can create another container with the same ubuntu image and check `/data.txt` does not exist with `docker run -it ubuntu ls /`.
Note that once it is executed, the new container will not be running because there is no command keeping it running (e.g., as the `tail -f` in the previous container).

To preserve the data from one container and restore it when another container is created, one can use volumes.

Volumes connect (i.e., mount) specific filesystem paths of the container to the host machine, changes in that ditectory are also seen on the host machines, if one then mounts the same directory to a new container, the data in that directory is preserved.

There are many types of volumes, one is called "named-volume", to create a named-volume, use `docker volume create todo-db`, then when creating a new container, use the flag `-v todo-db:/etc/todos` to create the mapping between `todo-db` volume and the container internal directory `/etc/todos`.

Once the container is reset, start it with the same flag `-v todo-db:/etc/todos` and the data is preserved.
On the host machine, the mount point can be checked with `docker volume inspect todo-db`.
To list all volumes, use `docker volume ls`

#### Ex 4: bind mounts

Named volume allows one to persist the database, but one cannot control where the database is stored on the host machine.
On the other hand, one can use bind mounts to contol the exact mountpoint on the host and use it to provide additional data in the container.
For instance, one can use bind mount to mount source code into the container and see the effect of code changes immediately.

For node-based applications, there are tools to watch code changes and restart the app.
To use this tool, one can start a new container with the `getting-started` image with:

```bash
docker run -dp 3000:3000 \
    -w /app -v "$(pwd):/app" \  
    # bind mount current directory (i.e., /app) from the host 
    # into the /app directory in the container
    # container does not need to have /app directory in advance
    node:12-alpine \
    sh -c "yarn install && yarn run dev"
```

Then, once some changes are made in the source code, one can use `docker logs <container-id>` to see the node tool restart the service, and one can see the changes take effect without rebuilding the image.
To enter the shell of the container, use `docker exec -it <container-id> sh`

Using bind mounts is a common practice in the local development stage.

#### Ex 5: multiple containers

Each container only starts one process, therefore when running multiple apps, it is better to use one container for each app and let multiple containers communicate.
As in a normal network, if 2 containers are in the same network, then they can directly communicate.

To create a network in docker, use `docker network create <network-name>`, then one can start a container and attach it to the network with the flag `--network` to specify the network group and `--network-alias` to assign an alias for the container's assigned ip.

For intance, to create a MySQL's container, one could run:
```bash
docker run -d \
     --network todo-app --network-alias mysql \
     -v todo-mysql-data:/var/lib/mysql \
     -e MYSQL_ROOT_PASSWORD=secret \
     -e MYSQL_DATABASE=todos \  # create a new database
     mysql:5.7
```
Note that with `-v` we used a volume `todo-mysql-data` without creating it first with `docker volume create` command, in this case Docker will create a named volume automatically.

To enter the MySQL, run `docker exec -it <container-id> mysql -u root -p` and enter the password.
The database `todos` can be found with `SHOW DATABASES;`.

Next, to connect to MySQL container, one can restart `getting-started` container to add it to the same network group and set the connection with several environmental variables:

```bash
docker run -dp 3000:3000 \
   -w /app -v "$(pwd):/app" \
   --network todo-app \
   -e MYSQL_HOST=mysql \  # network alias for mysql
   -e MYSQL_USER=root \
   -e MYSQL_PASSWORD=secret \
   -e MYSQL_DB=todos \
   node:12-alpine \
   sh -c "yarn install && yarn run dev"
```

Now, when adding new items to the database, one can also see it the MySQL container with `docker exec -it <container-id> mysql -p todos` and once inside the container run `select * from todo_items`.
Note that in practice it is unsafe to encode environment variables in one line, one should instead mount secrets as files in the container and during the startup point to those files.

To get the concrete ip address of a container instead of using the alias, one can use container `nicolaka/netshoot` with `docker run -it --network todo-app nicolaka/netshoot`.
This container works as a DNS server in the network group, and one can query the ip address with `dig mysql`.

#### Ex 6: docker compose

Docker compose can help creating multiple containers and their dependencies (e.g., app uses MySQL) in a much simpler way than above.
Instead of setting up a network group and creating each container separately, one can write the configs for diffent applications in a YML and use Docker compose to config them.

Docker compose should be by default already installed along with docker engine, if not, run `sudo apt install docker-compose-plugin` and `docker compose version` to make sure it has been installed.

Next, in the root of the app folder (e.g., `/app`), creat a `docker-compose.yml` file, and paste the following configs:

```yaml
version: "3.7"

services:
  app:
    image: node:12-alpine
    command: sh -c "yarn install && yarn run dev"
    ports:
      - 3000:3000
    working_dir: /app
    volumes:
      - ./:/app
    environment:
      MYSQL_HOST: mysql
      MYSQL_USER: root
      MYSQL_PASSWORD: secret
      MYSQL_DB: todos

  mysql:
    image: mysql:5.7
    volumes:
      - todo-mysql-data:/var/lib/mysql
    environment:
      MYSQL_ROOT_PASSWORD: secret
      MYSQL_DATABASE: todos

volumes:
  todo-mysql-data:
```

Basically, the compose file creates an application stack and contains all commands one needs in previous examples, including an `app` container, an `mysql` container and a named-volume.
Note that there is not network group creation in the file, because Docker compose will do that automatically.

To start the application stack, use `docker compose up -d`, then it will in the background (i.e., `-d`) creates a default network group for the stack, and creates 2 containers and 1 volume for the entire project (the project name depends on the YML file directory).
One can then check the overall log for all containers in one file with `docker compose logs -f` or `docker compose logs -f app` for a specific application, here the flag `-f` means "follow" so that it never terminates (e.g., like `tail -f`)

Note that Docker does not have built-in support to wait for another container to be ready before starting another container, one needs other tools to do this.

To tear down the project, run `docker compose down --volumes` to remove all containers and the named volumes, without the flag `--volumes`, named volumes will not be removed.

### Summary

1. create an image with Dockerfile: `docker build -t <image-name> .`

2. use an image to create a new container in the background: `docker run -d <image-name>`, if the image is not locally present, Docker will pull it from Docker hub
Note that the container will stop when there is no command running (e.g., defined in Dockerfile)

3. check a running container id with `docker ps` 

4. stop a container with `docker stop <container-id>`

5. stop and remove a container with `docker rm -f <container-id>`

6. to mount data between container and host machine, use `-v` tag when starting a container.
There are 2 main options to mount data: named volume is used to persist data, and bind mount is used to deploy changes to the container right away without rebuild the image

7. to communicate between multiple containers, first use `docker network create <network-name>` to create a network, and attach containers to the network during startup with the tag `--network <network-name>` and `--network-alias <dns-alias>`

8. to simplify the entire workflow, config compose file in the root of the project, and use `docker compose up -d` to start everything at once
