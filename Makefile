PROJECT = prometheus_rabbitmq_exporter

DEPS = rabbitmq_management prometheus prometheus_process_collector
dep_prometheus = git https://github.com/deadtrickster/prometheus.erl
dep_prometheus_process_collector = git https://github.com/deadtrickster/prometheus_process_collector

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
