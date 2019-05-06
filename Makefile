PROJECT = prometheus_rabbitmq_exporter
PROJECT_DESCRIPTION = RabbitMQ Prometheus.io metrics exporter
RABBITMQ_MINOR_VERSION = 3.7
# PROJECT_VERSION gets set in rabbitmq-components.mk to RABBITMQ_VERSION
RABBITMQ_VERSION = $(RABBITMQ_MINOR_VERSION).2.5
EZ = $(PROJECT)-$(PROJECT_VERSION)
PROJECT_APP_EXTRA_KEYS = {maintainers, ["Ilya Khaprov"]}, \
  {licenses, ["MIT"]}, \
  {links, [{"Github", "https://github.com/deadtrickster/prometheus_rabbitmq_exporter"}]}

ACCEPT_VERSION = 0.3.5
dep_accept = hex $(ACCEPT_VERSION)

PROMETHEUS_VERSION = 4.2.2
dep_prometheus = hex $(PROMETHEUS_VERSION)

PROMETHEUS_COWBOY_VERSION = 0.1.7
dep_prometheus_cowboy = hex $(PROMETHEUS_COWBOY_VERSION)

PROMETHEUS_HTTPD_VERSION = 2.1.10
dep_prometheus_httpd = hex $(PROMETHEUS_HTTPD_VERSION)

PROMETHEUS_PROCESS_COLLECTOR_VERSION = 1.4.3
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

RABBITMQ_BRANCH ?= v$(RABBITMQ_MINOR_VERSION).x
RABBITMQ_CURRENT_FETCH_URL ?= https://github.com/rabbitmq/

REBAR := $(CURDIR)/rebar3
REBAR_VERSION := 3.9.0
$(REBAR):
	@wget --output-document $(REBAR) https://github.com/erlang/rebar3/releases/download/$(REBAR_VERSION)/rebar3 && \
	chmod +x $(REBAR) && \
	$(REBAR) --version | grep $(REBAR_VERSION)

include rabbitmq-components.mk
include erlang.mk

.PHONY: distclean
distclean::
	@rm -fr tmp

DOCKER_IMAGE_VERSION = $(RABBITMQ_MINOR_VERSION)
DOCKER_BASE_IMAGE ?= rabbitmq:$(DOCKER_IMAGE_VERSION)-management

define BUILD_DOCKER_IMAGE
docker build \
  --pull \
  --build-arg FROM_IMAGE=$(DOCKER_BASE_IMAGE) \
  --build-arg PROMETHEUS_RABBITMQ_EXPORTER_VERSION=$(PROJECT_VERSION) \
  --tag deadtrickster/rabbitmq_prometheus:$(DOCKER_IMAGE_VERSION) .
endef

.PHONY: docker_build
docker_build:
	@$(BUILD_DOCKER_IMAGE)
.PHONY: docker_build_alpine
docker_build_alpine: DOCKER_IMAGE_VERSION = 3.7-alpine
docker_build_alpine: DOCKER_BASE_IMAGE = rabbitmq:3.7-management-alpine
docker_build_alpine: docker_build

define PUSH_DOCKER_IMAGE
docker push deadtrickster/rabbitmq_prometheus:$(DOCKER_IMAGE_VERSION)
endef

.PHONY: docker_push
docker_push:
	@$(PUSH_DOCKER_IMAGE)
.PHONY: docker_push_alpine
docker_push_alpine: DOCKER_IMAGE_VERSION = 3.7-alpine
docker_push_alpine: docker_push

define RUN_DOCKER_IMAGE
docker run --interactive --tty --publish=15672:15672 \
  deadtrickster/rabbitmq_prometheus:$(DOCKER_IMAGE_VERSION)
endef

.PHONY: docker_run
docker_run:
	@$(RUN_DOCKER_IMAGE)
.PHONY: docker_run_alpine
docker_run_alpine: DOCKER_IMAGE_VERSION = 3.7-alpine
docker_run_alpine: docker_run

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
	ln -sfnv $(CURDIR)/deps $(REBAR_DEPS_DIR)

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

define RUN_DOCKER_TEST_IMAGE
docker run \
  --tty \
  --detach \
  --name test_prometheus_rabbitmq_exporter \
  --publish 15672:15672 \
  deadtrickster/rabbitmq_prometheus:$(DOCKER_IMAGE_VERSION)
endef

define ENSURE_RABBIT_IN_DOCKER_IS_RUNNING
docker exec test_prometheus_rabbitmq_exporter \
  bash -c "while sleep 1; do rabbitmq-diagnostics check_port_listener 15672 2>/dev/null && break; done"
endef

define CLEAN_DOCKER_TEST_IMAGE
docker rm --force test_prometheus_rabbitmq_exporter
endef

define VERIFY_METRICS_API
curl --silent --verbose --fail localhost:15672/api/metrics
endef

.PHONY: test
test: ezs docker_build
	@$(CLEAN_DOCKER_TEST_IMAGE) ; \
	$(RUN_DOCKER_TEST_IMAGE) && \
	$(ENSURE_RABBIT_IN_DOCKER_IS_RUNNING) && \
	$(VERIFY_METRICS_API) && \
	$(CLEAN_DOCKER_TEST_IMAGE)
