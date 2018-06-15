PROJECT = prometheus_rabbitmq_exporter
PROJECT_DESCRIPTION = Prometheus.io exporter as a RabbitMQ Managment Plugin plugin
#PROJECT_MOD = prometheus_rabbitmq_exporter

DEPS = rabbitmq_management prometheus prometheus_httpd accept \
	prometheus_process_collector prometheus_cowboy
dep_prometheus = hex 3.5.1
dep_prometheus_process_collector = hex 1.3.1
dep_prometheus_httpd = hex 2.1.8
dep_accept = hex 0.3.3
dep_prometheus_cowboy = hex 0.1.4

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

.PHONY: docker_build docker_push docker_latest docker_latest_pure

docker_build:
	docker build -t deadtrickster/rabbitmq_prometheus\:3.7.2 .
	docker build -t deadtrickster/rabbitmq_prometheus\:latest .
	docker build -t deadtrickster/rabbitmq_prometheus\:3.7.2-pure -f Dockerfile.pure  .
	docker build -t deadtrickster/rabbitmq_prometheus\:latest-pure -f Dockerfile.pure  .

docker_push:
	docker push deadtrickster/rabbitmq_prometheus\:3.7.2
	docker push deadtrickster/rabbitmq_prometheus\:latest
	docker push deadtrickster/rabbitmq_prometheus\:3.7.2-pure
	docker push deadtrickster/rabbitmq_prometheus\:latest-pure

docker_latest:
	-docker run -p15672\:15672 deadtrickster/rabbitmq_prometheus\:latest

docker_pure:
	-docker run -p15672\:15672 deadtrickster/rabbitmq_prometheus\:latest-pure 

include rabbitmq-components.mk
include erlang.mk
