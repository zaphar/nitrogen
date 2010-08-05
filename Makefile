NITROGEN_VERSION=2.0.2

help:
	@echo 
	@echo "Usage: "
	@echo "       ./make {compile|clean}"        
	@echo
	@echo "       ./make {rel_inets|package_inets}"  
	@echo "       ./make {rel_mochiweb|package_mochiweb}"
	@echo "       ./make {rel_yaws|package_yaws}"
	@echo
	@echo "       ./make package_docs"
	@echo
	@echo

compile:
	@(cd ./apps/nitrogen; make compile)
	@(cd ./apps/simple_bridge; make compile)
	@(cd ./apps/nprocreg; make compile)


debug:
	mkdir -p ebin
	mkdir -p test/ebin
	ERL_LIBS=deps erl -noinput -run make all debug_info -run init stop

clean:
	@(cd ./apps/nitrogen; make clean)
	@(cd ./apps/simple_bridge; make clean)
	@(cd ./apps/nprocreg; make clean)


# INETS

rel_inets: compile
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/inets.config rel/reltool.config
	@(make rel_inner)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Inets.

package_inets: rel_inets
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-inets.tar.gz



# MOCHIWEB

rel_mochiweb: compile compile_mochiweb
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/mochiweb.config rel/reltool.config
	@(make rel_inner)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Mochiweb.

compile_mochiweb:
	@(cd apps/mochiweb; make all)

package_mochiweb: rel_mochiweb
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-mochiweb.tar.gz


# YAWS

rel_yaws: compile compile_yaws
	@rm -rf rel/nitrogen
	@rm -rf rel/reltool.config
	@ln rel/yaws.config rel/reltool.config
	@(make rel_inner)
	@echo Generated a self-contained Nitrogen project
	@echo in 'rel/nitrogen', configured to run on Yaws.

compile_yaws: 
	@(cd apps/yaws-1.87; ./configure --disable-sendfile; make)

package_yaws: rel_yaws
	mkdir -p ./builds
	tar -C rel -c nitrogen | gzip > ./builds/nitrogen-${NITROGEN_VERSION}-yaws.tar.gz


# Create an Inets release dependent upon the built in Erlang installation.

dependent: rel_inets
	@rm -rf rel/nitrogen/erts-*
	@mv rel/nitrogen/lib rel/nitrogen/lib2
	@mkdir rel/nitrogen/lib
	@cp -r rel/nitrogen/lib2/nitrogen* rel/nitrogen/lib
	@cp -r rel/nitrogen/lib2/nprocreg* rel/nitrogen/lib
	@cp -r rel/nitrogen/lib2/simple_bridge* rel/nitrogen/lib
	@rm -rf rel/nitrogen/lib2
	@cp rel/overlay_dependent/bin/nitrogen rel/nitrogen/bin/
	@mkdir -p rel/nitrogen/erts/bin
	@cp rel/overlay/erts-vsn/bin/nodetool rel/nitrogen/erts/bin

# SHARED

rel_inner:
	@(cd rel; ./rebar generate)
	@(cd rel/nitrogen; make)
	@printf "Nitrogen Version:\n${NITROGEN_VERSION}\n\n" > rel/nitrogen/BuildInfo.txt
	@echo "Built On (uname -v):" >> rel/nitrogen/BuildInfo.txt
	@uname -v >> rel/nitrogen/BuildInfo.txt
	@cp -r ./doc rel/nitrogen/doc
	@cp -r ./apps/nitrogen/www rel/nitrogen/site/static/nitrogen
	@rm -rf rel/reltool.config	

rellink:  
	$(foreach app,$(wildcard apps/*), rm -rf rel/nitrogen/lib/$(shell basename $(app))* && ln -sf $(abspath $(app)) rel/nitrogen/lib;)
