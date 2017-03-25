PROJECT = prometheus_rabbitmq_exporter
PROJECT_DESCRIPTION = Prometheus.io exporter as a RabbitMQ Managment Plugin plugin

DEPS = rabbitmq_management prometheus prometheus_process_collector prometheus_httpd accept
dep_prometheus = git https://github.com/deadtrickster/prometheus.erl
dep_prometheus_process_collector = git https://github.com/deadtrickster/prometheus_process_collector
dep_prometheus_httpd = git https://github.com/deadtrickster/prometheus-httpd
dep_accept = git https://github.com/deadtrickster/accept

DEP_PLUGINS = rabbit_common/mk/rabbitmq-build.mk \
	      rabbit_common/mk/rabbitmq-dist.mk \
	      rabbit_common/mk/rabbitmq-run.mk \
	      rabbit_common/mk/rabbitmq-test.mk \
	      rabbit_common/mk/rabbitmq-tools.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk

NO_AUTOPATCH = 0
