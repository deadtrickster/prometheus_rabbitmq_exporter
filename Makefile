PROJECT = prometheus_rabbitmq_exporter
PROJECT_DESCRIPTION = RabbitMQ Prometheus.io metrics exporter
# PROJECT_VERSION gets set in rabbitmq-components.mk to RABBITMQ_VERSION
RABBITMQ_VERSION = 3.7.$(shell date +'%Y%m%d')
EZ = $(PROJECT)-$(PROJECT_VERSION)
PROJECT_APP_EXTRA_KEYS = {maintainers, ["Ilya Khaprov"]}, \
  {licenses, ["MIT"]}, \
  {links, [{"Github", "https://github.com/deadtrickster/prometheus_rabbitmq_exporter"}]}

ACCEPT_VERSION = 0.3.4
dep_accept = hex $(ACCEPT_VERSION)

PROMETHEUS_VERSION = 4.2.2
dep_prometheus = hex $(PROMETHEUS_VERSION)

PROMETHEUS_COWBOY_VERSION = 0.1.7
dep_prometheus_cowboy = hex $(PROMETHEUS_COWBOY_VERSION)

PROMETHEUS_HTTPD_VERSION = 2.1.10
dep_prometheus_httpd = hex $(PROMETHEUS_HTTPD_VERSION)

PROMETHEUS_PROCESS_COLLECTOR_VERSION = 1.4.0
dep_prometheus_process_collector = hex $(PROMETHEUS_PROCESS_COLLECTOR_VERSION)

DEPS = rabbit rabbitmq_management  \
       prometheus prometheus_cowboy prometheus_httpd

# We do not want these deps defined as applications in app
BUILD_DEPS = rabbit_common rabbitmq_management_agent accept prometheus_process_collector

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

RABBITMQ_BRANCH ?= v3.7.x
RABBITMQ_CURRENT_FETCH_URL ?= https://github.com/rabbitmq/

REBAR := $(CURDIR)/rebar3
REBAR_VERSION := 3.9.0
$(REBAR):
	@wget --output-document $(REBAR) https://github.com/erlang/rebar3/releases/download/$(REBAR_VERSION)/rebar3 && \
	chmod +x $(REBAR) && \
	$(REBAR) --version | grep $(REBAR_VERSION)

include rabbitmq-components.mk
include erlang.mk

.PHONY: up docker_build docker_push docker_latest docker_pure docker_alpine

docker_build:
	docker build -t deadtrickster/rabbitmq_prometheus\:3.7.8 .
	docker build -t deadtrickster/rabbitmq_prometheus\:latest .
	docker build -t deadtrickster/rabbitmq_prometheus\:3.7.8-pure -f Dockerfile.pure  .
	docker build -t deadtrickster/rabbitmq_prometheus\:latest-pure -f Dockerfile.pure  .
	docker build -t deadtrickster/rabbitmq_prometheus\:3.7.8-alpine -f Dockerfile.alpine  .
	docker build -t deadtrickster/rabbitmq_prometheus\:latest-alpine -f Dockerfile.alpine  .

docker_push:
	docker push deadtrickster/rabbitmq_prometheus\:3.7.8
	docker push deadtrickster/rabbitmq_prometheus\:latest
	docker push deadtrickster/rabbitmq_prometheus\:3.7.8-pure
	docker push deadtrickster/rabbitmq_prometheus\:latest-pure
	docker push deadtrickster/rabbitmq_prometheus\:3.7.8-alpine
	docker push deadtrickster/rabbitmq_prometheus\:latest-alpine

docker_latest:
	-docker run -p15672\:15672 deadtrickster/rabbitmq_prometheus\:latest

docker_pure:
	-docker run -p15672\:15672 deadtrickster/rabbitmq_prometheus\:latest-pure

docker_alpine:
	-docker run -p15672\:15672 deadtrickster/rabbitmq_prometheus\:latest-alpine

.PHONY: up
up: $(abspath .)+up $(DEPS:%=$(DEPS_DIR)/%+up) $(BUILD_DEPS:%=$(DEPS_DIR)/%+up)
	@:

%+up: fetch-deps
	$(exec_verbose) cd $*; \
	git fetch -p && \
	if [ '$(RABBITMQ_BRANCH)' ]; then \
		git checkout $(RABBITMQ_BRANCH) || : ; \
	fi && \
	if git symbolic-ref -q HEAD >/dev/null; then \
		branch=$$(git symbolic-ref --short HEAD); \
		remote=$$(git config branch.$$branch.remote); \
		merge=$$(git config branch.$$branch.merge | sed 's,refs/heads/,,'); \
		if [ "$$remote" -a "$$merge" ]; then \
			git merge --ff-only "$$remote/$$merge"; \
		fi; \
	fi && \
	echo

REBAR_DEPS_DIR := _build/default/lib
$(REBAR_DEPS_DIR): up
	@mkdir -p _build/default && \
	ln -shfv $(CURDIR)/deps $(REBAR_DEPS_DIR)

tmp:
	@mkdir -p tmp

tmp/accept-$(ACCEPT_VERSION).ez: $(REBAR) $(REBAR_DEPS_DIR) tmp
	@cd $(REBAR_DEPS_DIR)/accept && \
	$(REBAR) archive && \
	mv accept-$(ACCEPT_VERSION).ez $(CURDIR)/tmp/

tmp/prometheus-$(PROMETHEUS_VERSION).ez: $(REBAR) $(REBAR_DEPS_DIR) tmp
	@cd $(REBAR_DEPS_DIR)/prometheus && \
	$(REBAR) archive && \
	mv prometheus-$(PROMETHEUS_VERSION).ez $(CURDIR)/tmp/

tmp/prometheus_cowboy-$(PROMETHEUS_COWBOY_VERSION).ez: $(REBAR) $(REBAR_DEPS_DIR) tmp
	@cd $(REBAR_DEPS_DIR)/prometheus_cowboy && \
	$(REBAR) archive && \
	mv prometheus_cowboy-$(PROMETHEUS_COWBOY_VERSION).ez $(CURDIR)/tmp/

tmp/prometheus_httpd-$(PROMETHEUS_HTTPD_VERSION).ez: $(REBAR) $(REBAR_DEPS_DIR) tmp
	@cd $(REBAR_DEPS_DIR)/prometheus_httpd && \
	$(REBAR) archive && \
	mv prometheus_httpd-$(PROMETHEUS_HTTPD_VERSION).ez $(CURDIR)/tmp/

tmp/prometheus_process_collector-$(PROMETHEUS_PROCESS_COLLECTOR_VERSION).ez: $(REBAR) $(REBAR_DEPS_DIR) tmp
	@cd $(REBAR_DEPS_DIR)/prometheus_process_collector && \
	$(REBAR) archive && \
	mv prometheus_process_collector-$(PROMETHEUS_PROCESS_COLLECTOR_VERSION).ez $(CURDIR)/tmp

tmp/$(EZ).ez: up app tmp
	@rm -fr $(EZ) && mkdir $(EZ) && \
	cp -r ebin include priv $(EZ) && \
	rm -f $(EZ).ez && \
	zip --move --recurse-paths --test $(EZ).ez $(EZ) && \
	mv $(EZ).ez tmp/

.PHONY: ezs
ezs:: tmp/accept-$(ACCEPT_VERSION).ez
ezs:: tmp/prometheus-$(PROMETHEUS_VERSION).ez
ezs:: tmp/prometheus_cowboy-$(PROMETHEUS_COWBOY_VERSION).ez
ezs:: tmp/prometheus_httpd-$(PROMETHEUS_HTTPD_VERSION).ez
ezs:: tmp/prometheus_process_collector-$(PROMETHEUS_PROCESS_COLLECTOR_VERSION).ez
ezs:: tmp/$(EZ).ez
