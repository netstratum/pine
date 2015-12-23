.PHONY: all clean compile deps clean_app release start ping stop attach console console_clean build_rpm test test_console

rebar=rebar

all: deps compile
increment: compile release console
compile:
	@$(rebar) compile
deps:
	@mkdir -p deps
	@$(rebar) get-deps
clean:
	@$(rebar) -r clean
release:
	@$(rebar) -q generate
start:
	@rel/pine/bin/pine start
ping:
	@rel/pine/bin/pine ping
stop:
	@rel/pine/bin/pine stop
attach:
	@rel/pine/bin/pine attach
console:
	@rel/pine/bin/pine console
console_clean:
	@rel/pine/bin/pine console_clean
test:
	@cd test;$(MAKE) deps compile
test_clean:
	@cd test;$(MAKE) clean
test_console:
	@cd test;$(MAKE) console
test_increment:
	@cd test;$(MAKE) increment
rpm:
	@mkdir -p pkg/rpm/
	@fpm -s dir -t rpm -n pine -v 0.1 -C rel -p pine-VERSION_ARCH.rpm --prefix /opt --description "PIN generation and management Engine" --rpm-user pine --rpm-group pine --rpm-compression gzip --before-install pkg/files/before_install.sh --epoch 1 --after-install pkg/files/after_install.sh --after-remove pkg/files/after_remove.sh -p pkg/rpm/ pine
deb:
	@mkdir -p pkg/deb/
	@fpm -s dir -t deb -n pine -v 0.1 -C rel -p pine-VERSION_ARCH.rpm --prefix /opt --description "PIN generation and management Engine" --deb-user pine --deb-group pine --deb-compression gz --before-install pkg/files/before_install.sh --after-install pkg/files/after_install.sh --after-remove pkg/files/after_remove.sh -p pkg/deb/ pine

