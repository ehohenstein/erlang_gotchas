
SOURCES=$(wildcard *.erl)
BEAMS=$(patsubst %.erl, %.beam, $(SOURCES))
DOCKERFILES=$(wildcard Dockerfile.*)

all: $(BEAMS) containers

%.beam: %.erl
	erlc $<

clean:
	rm $(BEAMS)

containers: $(BEAMS) $(DOCKERFILES)
	docker build -t bionic-erlang -f Dockerfile.erlang .
	docker build -t sink -f Dockerfile.sink .
	docker build -t source -f Dockerfile.source .

erl-net:
	docker network create -d bridge erl-net

start-sink:
	docker run --hostname sinkhost --name sinkhost --net erl-net -it sink
	docker rm sinkhost

pause-sink:
	docker pause sinkhost

resume-sink:
	docker unpause sinkhost

start-source:
	docker run --hostname sourcehost --name sourcehost --link sinkhost --net erl-net -it source
	docker rm sourcehost

start-source-noconnect:
	docker run --hostname sourcehost --name sourcehost --link sinkhost --net erl-net -e SOURCE_FLAGS=noconnect -it source
	docker rm sourcehost

start-source-nosuspend:
	docker run --hostname sourcehost --name sourcehost --link sinkhost --net erl-net -e SOURCE_FLAGS=nosuspend -it source
	docker rm sourcehost

start-source-failfast:
	docker run --hostname sourcehost --name sourcehost --link sinkhost --net erl-net -e SOURCE_FLAGS=noconnect,nosuspend -it source
	docker rm sourcehost

docker-clean:
	docker network rm erl-net

.PHONY: all containers clean
