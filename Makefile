PROJECT = prometheus_rabbitmq_exporter
PROJECT_DESCRIPTION = Prometheus.io exporter as a RabbitMQ Managment Plugin plugin
#PROJECT_MOD = prometheus_rabbitmq_exporter

DEPS = rabbitmq_management prometheus prometheus_httpd accept \
	prometheus_process_collector prometheus_cowboy
dep_prometheus = hex 3.4.4
dep_prometheus_process_collector = hex 1.3.0
dep_prometheus_httpd = hex 2.1.8
dep_accept = hex 0.3.3
dep_prometheus_cowboy = hex 0.1.4

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
