
SHELL := /bin/bash
.ONESHELL:
.SHELLFLAGS := -c
BIN_DIR := bin

# Directory for all stack traces (program 1 and program 2)
STACK_TRACE := STACK_TRACE

# Mark all targets as phony
.PHONY: all all1 all2 all3 bin clear install_compilers \
  ada clean csharp clojure crystal d elixir erlang fsharp go haskell java javascript \
  julia kotlin lua nim ocaml odin perl php python r ruby rust scala smalltalk swift v zig \
  \
  ada1 clean1 csharp1 clojure1 crystal1 d1 elixir1 erlang1 fsharp1 go1 haskell1 java1 javascript1 \
  julia1 kotlin1 lua1 nim1 ocaml1 odin1 perl1 php1 python1 r1 ruby1 rust1 scala1 smalltalk1 swift1 v1 zig1 \
  \
  ada2 clean2 csharp2 clojure2 crystal2 d2 elixir2 erlang2 fsharp2 go2 haskell2 java2 javascript2 \
  julia2 kotlin2 lua2 nim2 ocaml2 odin2 perl2 php2 python2 r2 ruby2 rust2 scala2 smalltalk2 swift2 v2 zig2 \
  \
  python3 ada3 csharp3 clojure3 crystal3 d3 elixir3 erlang3 fsharp3 go3 haskell3 \
  java3 javascript3 julia3 kotlin3 lua3 nim3 ocaml3 perl3 php3 r3 ruby3 rust3 \
  scala3 smalltalk3 swift3 v3 clean3 zig3 odin3 \
  \
  python4 ada4 csharp4 clojure4 crystal4 d4 elixir4 erlang4 fsharp4 go4 haskell4 \
  java4 javascript4 julia4 kotlin4 lua4 nim4 ocaml4 perl4 php4 r4 ruby4 rust4 \
  scala4 smalltalk4 swift4 v4 clean4 zig4 odin4 \
  versions

all: all1 all2 all3 all4
	@echo There are now $$(ls $(STACK_TRACE) | wc -l) files in $(STACK_TRACE)

# Aggregate targets
all1: bin ada1 csharp1 clojure1 clean1 crystal1 d1 elixir1 erlang1 fsharp1 go1 haskell1 java1 javascript1 \
	julia1 kotlin1 lua1 nim1 ocaml1 odin1 perl1 php1 python1 r1 ruby1 rust1 scala1 smalltalk1 swift1 v1 zig1

all2: bin ada2 csharp2 clojure2 crystal2 d2 elixir2 erlang2 fsharp2 go2 haskell2 java2 javascript2 \
	julia2 kotlin2 lua2 nim2 ocaml2 odin2 perl2 php2 python2 r2 ruby2 rust2 scala2 clean2 smalltalk2 swift2 v2 zig2

all3: python3 ada3 csharp3 clojure3 clean3 crystal3 d3 elixir3 erlang3 fsharp3 go3 haskell3 \
	java3 javascript3 julia3 kotlin3 lua3 nim3 ocaml3 odin3 perl3 php3 r3 ruby3 rust3 \
	scala3 smalltalk3 swift3 v3 clean3 zig3 odin3

all4: python4 ada4 csharp4 clojure4 clean4 crystal4 d4 elixir4 erlang4 fsharp4 go4 haskell4 \
	java4 javascript4 julia4 kotlin4 lua4 nim4 ocaml4 odin4 perl4 php4 r4 ruby4 rust4 \
	scala4 smalltalk4 swift4 v4 clean4 zig4 odin4

# Common setup
bin:
	@mkdir -p bin bin/java bin/scala $(STACK_TRACE)

# ===================== Program I  =====================
ada1: bin
	@if command -v gnatmake >/dev/null; then \
	  gnatmake -g -o bin/ada.out ada/main.adb > /dev/null; \
	  ./bin/ada.out > $(STACK_TRACE)/stack1.ada.txt 2>&1 || true; \
	  rm -f b~main.ali main.ali main.o b~main.o b~main.ali b~main.o b~main.adb b~main.ads
	else echo "gnatmake not installed; skipping"; fi

csharp1: | $(STACK_TRACE)
	@if command -v docker >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  docker run --rm \
	    -u $$(id -u):$$(id -g) \
	    -e HOME=/tmp \
	    -e DOTNET_CLI_HOME=/tmp \
	    -e NUGET_PACKAGES=/tmp/nugetpackages \
	    -e DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1 \
	    -e DOTNET_CLI_TELEMETRY_OPTOUT=1 \
	    -e DOTNET_NOLOGO=1 \
	    -v "$$(pwd)/csharp:/src" \
	    -v "$$(pwd)/$(STACK_TRACE):/out" \
	    -w /src \
	    mcr.microsoft.com/dotnet/sdk:8.0 \
	    bash -lc 'mkdir -p /tmp/nugetpackages; \
	              (dotnet restore >/dev/null 2>&1 || true); \
	              dotnet build -c Debug >/dev/null; \
	              ( ulimit -c 0; dotnet run -c Debug ) > /out/stack1.csharp.txt 2>&1' \
	    || true; \
		rm -rf csharp/bin csharp/obj \
	else echo "docker not installed; skipping C#"; fi

# Put both clm and cocl on path https://clean.cs.ru.nl/Download_Clean
clean1:
	@if command -v clm >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  ( cd clean && clm -ns -ms -tst -h 100M -ci Main -o ../bin/clean.out ); \
	  ./bin/clean.out > "$(STACK_TRACE)/stack1.clean.txt" 2>&1 || true; \
	  rm -fr "clean/Clean System Files"
	else echo "Clean compiler (clm) not installed; skipping see https://clean.cs.ru.nl/Download_Clean"; fi




clojure1: bin
	@if command -v clojure >/dev/null; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  errlog=$$(mktemp); outlog=$$(mktemp); \
	  ( cd clojure && clojure -e '(load-file "main.clj")' ) > "$$outlog" 2> "$$errlog" || true; \
	  report=$$(awk '/^Full report at:/{getline; print $$0}' "$$errlog" | head -n1); \
	  if [ -n "$$report" ] && [ -f "$$report" ]; then \
	    cat "$$report" > "$(STACK_TRACE)/stack1.clojure.txt"; \
	  else \
	    cat "$$errlog" "$$outlog" > "$(STACK_TRACE)/stack1.clojure.txt"; \
	  fi; \
	  rm -f "$$errlog" "$$outlog"; \
	else \
	  echo "clojure not installed; skipping"; \
	fi


crystal1: bin
	@if command -v crystal >/dev/null; then \
	  crystal build --debug -o bin/crystal.out crystal/main.cr; \
	  ./bin/crystal.out > $(STACK_TRACE)/stack1.crystal.txt 2>&1 || true; \
	else echo "crystal not installed; skipping"; fi

d1: bin
	@if command -v dmd >/dev/null; then \
	  dmd -g -of=bin/d.out d/main.d; \
	  ./bin/d.out > $(STACK_TRACE)/stack1.d.txt 2>&1 || true; \
	else echo "dmd not installed; skipping"; fi

elixir1: bin
	@if command -v elixir >/dev/null; then \
	  elixir elixir/main.exs > $(STACK_TRACE)/stack1.elixir.txt 2>&1 || true; \
	else echo "elixir not installed; skipping"; fi

erlang1: bin
	@if command -v escript >/dev/null; then \
	  escript erlang/main.escript > $(STACK_TRACE)/stack1.erlang.txt 2>&1 || true; \
	else echo "escript not installed; skipping"; fi

fsharp1: | $(STACK_TRACE)
	@if command -v fsharpi >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  ( ulimit -c 0; fsharpi --exec fsharp/main.fsx ) > "$(STACK_TRACE)/stack1.fsharp.txt" 2>&1 || true; \
	else echo "fsharpi not installed; skipping"; fi

go1: bin
	@if command -v go >/dev/null; then \
	  go build -o bin/go.out go/main.go; \
	  ./bin/go.out > $(STACK_TRACE)/stack1.go.txt 2>&1 || true; \
	else echo "go not installed; skipping"; fi

haskell1: bin
	@if command -v ghc >/dev/null; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  # Try profiling build for +RTS -xc
	  if ghc -O0 -g -rtsopts -prof -fprof-auto -o bin/haskell.out haskell/Main.hs >/dev/null 2>&1; then \
	    ./bin/haskell.out +RTS -xc > "$(STACK_TRACE)/stack1.haskell.txt" 2>&1 || true; \
	    rm -f haskell/Main.o haskell/Main.hi
	  else \
	    echo "ghc profiling libs not available; install with apt install ghc-prof but for now falling back to non-profiling build" >&2; \
	    ghc -O0 -g -rtsopts -o bin/haskell.out haskell/Main.hs >/dev/null 2>&1 || true; \
	    ./bin/haskell.out > "$(STACK_TRACE)/stack1.haskell.txt" 2>&1 || true; \
	    rm -f haskell/Main.o haskell/Main.hi
	  fi; \
	else \
	  echo "ghc not installed; skipping"; \
	fi

java1: bin
	@if command -v javac >/dev/null; then \
	  mkdir -p bin/java.out; \
	  javac -g -d bin/java.out java/Main.java; \
	  java -cp bin/java.out Main > $(STACK_TRACE)/stack1.java.txt 2>&1 || true; \
	else echo "javac not installed; skipping"; fi

javascript1: bin
	@if command -v node >/dev/null; then \
	  node javascript/main.mjs > $(STACK_TRACE)/stack1.javascript.txt 2>&1 || true; \
	else echo "node not installed; skipping"; fi

julia1: bin
	@if command -v julia >/dev/null; then \
	  julia julia/main.jl > $(STACK_TRACE)/stack1.julia.txt 2>&1 || true; \
	else echo "julia not installed; skipping"; fi

define FIND_JDK_SNIPPET
  JDK_HOME=""; \
  pick_jdk() { \
    CAND="$$1"; \
    if [ -n "$$CAND" ] && [ -x "$$CAND/bin/javac" ]; then JDK_HOME="$$CAND"; return 0; fi; \
    return 1; \
  }; \
  # 1) user override
  pick_jdk "$$KOTLIN_JDK_HOME" || \
  # 2) JAVA_HOME
  pick_jdk "$$JAVA_HOME" || \
  # 3) resolve from javac on PATH
  if command -v javac >/dev/null 2>&1; then \
    JAVAC_PATH="$$(command -v javac)"; \
    # handle symlinks like /usr/bin/javac -> /etc/alternatives/javac -> /usr/lib/jvm/.../bin/javac
    REAL_JAVAC="$$(readlink -f "$$JAVAC_PATH" 2>/dev/null || echo "$$JAVAC_PATH")"; \
    CAND="$$(dirname "$$(dirname "$$REAL_JAVAC")")"; \
    pick_jdk "$$CAND" || true; \
  fi; \
  # 4) common locations (Debian/Ubuntu/Fedora/RHEL)
  if [ -z "$$JDK_HOME" ]; then \
    for J in /usr/lib/jvm/* /usr/lib64/jvm/*; do \
      [ -x "$$J/bin/javac" ] && { JDK_HOME="$$J"; break; }; \
    done; \
  fi; \
  # Optional: gate to a sane version (11–21). Only warn; don't fail.
  if [ -n "$$JDK_HOME" ] && "$$JDK_HOME/bin/java" -version 2>&1 | grep -Eq '"1?[1-9]|2[01]\.'; then \
    :; \
  fi
endef

kotlin1: bin
	@if command -v kotlinc >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  $(FIND_JDK_SNIPPET); \
	  err=$$(mktemp); \
	  if [ -n "$$JDK_HOME" ]; then \
	    KOTLINC_JDK_FLAG="-jdk-home $$JDK_HOME"; \
	    JAVA_BIN="$$JDK_HOME/bin/java"; \
	  else \
	    KOTLINC_JDK_FLAG=""; \
	    JAVA_BIN="$$(command -v java || echo java)"; \
	  fi; \
	  # Compile
	  if ! kotlinc $$KOTLINC_JDK_FLAG kotlin/Main.kt -include-runtime -d bin/kotlin.jar 2> "$$err"; then \
	    if grep -Eq 'ASM9|Unsupported class file major version' "$$err"; then \
	      { echo "Kotlin compiler is too old for this JDK."; \
	        echo "Upgrade kotlinc (e.g. SDKMAN: 'sdk install kotlin 1.9.24' && 'sdk use kotlin 1.9.24')."; \
	      } > "$(STACK_TRACE)/stack1.kotlin.txt"; \
	    else \
	      cat "$$err" > "$(STACK_TRACE)/stack1.kotlin.txt"; \
	    fi; \
	    rm -f "$$err"; \
	    exit 0; \
	  fi; \
	  rm -f "$$err"; \
	  # Run (no manifest; use -cp and entry class)
	  "$$JAVA_BIN" -cp bin/kotlin.jar Main > "$(STACK_TRACE)/stack1.kotlin.txt" 2>&1 || true; \
	else \
	  echo "kotlinc not installed; skipping"; \
	fi

lua1: bin
	@if command -v lua >/dev/null; then \
	  lua lua/main.lua > $(STACK_TRACE)/stack1.lua.txt 2>&1 || true; \
	else echo "lua not installed; skipping"; fi

nim1: bin
	@if command -v nim >/dev/null; then \
	  nim c -d:debug --out:bin/nim.out nim/main.nim > /dev/null; \
	  ./bin/nim.out > $(STACK_TRACE)/stack1.nim.txt 2>&1 || true; \
	else echo "nim not installed; skipping"; fi

ocaml1: bin
	@if command -v ocamlopt >/dev/null; then \
	  ocamlopt -g -o bin/ocaml.out ocaml/main.ml; \
	  OCAMLRUNPARAM=b ./bin/ocaml.out > $(STACK_TRACE)/stack1.ocaml.txt 2>&1 || true; \
	  rm -f ocaml/main.cmi ocaml/main.cmx ocaml/main.o \
	else echo "ocamlopt not installed; skipping"; fi

odin1:
	@if command -v odin >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  odin build  odin/main.odin -debug -file -out:bin/odin.out; \
	  ( ulimit -c 0; ./bin/odin.out ) > "$(STACK_TRACE)/stack1.odin.txt" 2>&1 || true; \
	else echo "odin not installed; skipping"; fi

perl1: bin
	@if command -v perl >/dev/null; then \
	  perl perl/main.pl > $(STACK_TRACE)/stack1.perl.txt 2>&1 || true; \
	else echo "perl not installed; skipping"; fi

php1: bin
	@if command -v php >/dev/null; then \
	  php php/main.php > $(STACK_TRACE)/stack1.php.txt 2>&1 || true; \
	else echo "php not installed; skipping"; fi

python1: bin
	@if command -v python3 >/dev/null; then \
	  python3 -B python/main.py > $(STACK_TRACE)/stack1.python.txt 2>&1 || true; \
	  rm -fr __pycache__		\ 
	else echo "python3 not installed; skipping"; fi

r1: bin
	@if command -v Rscript >/dev/null; then \
	  Rscript r/main.R > $(STACK_TRACE)/stack1.r.txt 2>&1 || true; \
	else echo "Rscript not installed; skipping"; fi

ruby1: bin
	@if command -v ruby >/dev/null; then \
	  ruby ruby/main.rb > $(STACK_TRACE)/stack1.ruby.txt 2>&1 || true; \
	else echo "ruby not installed; skipping"; fi

rust1: bin
	@if command -v rustc >/dev/null; then \
	  rustc -g -C debuginfo=2 -o bin/rust.out rust/main.rs; \
	  RUST_BACKTRACE=1 ./bin/rust.out > $(STACK_TRACE)/stack1.rust.txt 2>&1 || true; \
	else echo "rustc not installed; skipping"; fi

scala1: bin
	@if command -v scalac >/dev/null; then \
	  scalac -g:vars -d bin scala/Main.scala; \
	  scala -cp bin Main > $(STACK_TRACE)/stack1.scala.txt 2>&1 || true; \
	else echo "scalac not installed; skipping"; fi



smalltalk1: bin
	@if command -v gst >/dev/null; then \
	  gst smalltalk/main.st > $(STACK_TRACE)/stack1.smalltalk.txt 2>&1 || true; \
	else echo "gnu smalltalk not installed; skipping"; fi

swift1:
	@if command -v swiftc >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  swiftc -g swift/main.swift -o bin/swift.out; \
	  ./bin/swift.out > "$(STACK_TRACE)/stack1.swift.txt" 2>&1 || true; \
	else echo "swiftc not installed; skipping"; fi

v1: bin
	@if command -v v >/dev/null; then \
	  v -g -o bin/v.out v/main.v; \
	  ./bin/v.out > $(STACK_TRACE)/stack1.v.txt 2>&1 || true; \
	else echo "v not installed; skipping"; fi


zig1: bin
	@if command -v zig >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  zig build-exe -O Debug -femit-bin=bin/zig.out zig/main.zig; \
	  ( ulimit -c 0; ZIG_BACKTRACE=full ./bin/zig.out ) > "$(STACK_TRACE)/stack1.zig.txt" 2>&1 || true; \
	else echo "zig not installed; skipping"; fi



# ===================== Program II =====================
ada2: bin
	@if command -v gnatmake >/dev/null; then \
	  gnatmake -g -o bin/ada2 ada/main2.adb > /dev/null; \
	  ./bin/ada2 > $(STACK_TRACE)/stack2.ada.txt 2>&1 || true; \
	rm -f b~main2.ali main2.ali main2.o b~main2.o b~main2.ali b~main2.o b~main2.adb b~main2.ads
	else echo "gnatmake not installed; skipping"; fi

csharp2: | $(STACK_TRACE)
	@if command -v docker >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  docker run --rm \
	    -u $$(id -u):$$(id -g) \
	    -e HOME=/tmp \
	    -e DOTNET_CLI_HOME=/tmp \
	    -e NUGET_PACKAGES=/tmp/nugetpackages \
	    -e DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1 \
	    -e DOTNET_CLI_TELEMETRY_OPTOUT=1 \
	    -e DOTNET_NOLOGO=1 \
	    -v "$$(pwd)/csharp2:/src" \
	    -v "$$(pwd)/$(STACK_TRACE):/out" \
	    -w /src \
	    mcr.microsoft.com/dotnet/sdk:8.0 \
	    bash -lc 'mkdir -p /tmp/nugetpackages; \
	              (dotnet restore >/dev/null 2>&1 || true); \
	              dotnet build -c Debug >/dev/null; \
	              ( ulimit -c 0; dotnet run -c Debug ) > /out/stack2.csharp.txt 2>&1' \
	    || true; \
		rm -rf csharp2/bin csharp2/obj \
	else echo "docker not installed; skipping C#"; fi

clojure2: bin
	@if command -v clojure >/dev/null; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  errlog=$$(mktemp); outlog=$$(mktemp); \
	  ( cd clojure && clojure -e '(load-file "main2.clj")' ) > "$$outlog" 2> "$$errlog" || true; \
	  report=$$(awk '/^Full report at:/{getline; print $$0}' "$$errlog" | head -n1); \
	  if [ -n "$$report" ] && [ -f "$$report" ]; then \
	    cat "$$report" > "$(STACK_TRACE)/stack2.clojure.txt"; \
	  else \
	    cat "$$errlog" "$$outlog" > "$(STACK_TRACE)/stack2.clojure.txt"; \
	  fi; \
	  rm -f "$$errlog" "$$outlog"; \
	else \
	  echo "clojure not installed; skipping"; \
	fi

clean2:
	@if command -v clm >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  ( cd clean && clm -ns -ms -tst -h 100M -ci Main3 -o ../bin/clean2 ); \
	  ./bin/clean2 > "$(STACK_TRACE)/stack2.clean.txt" 2>&1 || true; \
	  rm -fr "clean/Clean System Files"
	else echo "Clean compiler (clm) not installed; skipping see https://clean.cs.ru.nl/Download_Clean"; fi


crystal2: bin
	@if command -v crystal >/dev/null; then \
	  crystal build --debug -o bin/crystal2 crystal/main2.cr; \
	  ./bin/crystal2 > $(STACK_TRACE)/stack2.crystal.txt 2>&1 || true; \
	else echo "crystal not installed; skipping"; fi

d2: bin
	@if command -v dmd >/dev/null 2>&1; then \
	  dmd -g -of=bin/d2 d/main2.d; \
	  bash -lc 'ulimit -c 0; exec >"$(STACK_TRACE)/stack2.d.txt" 2>&1; ./bin/d2; exit 0'; \
	else echo "dmd not installed; skipping"; fi


elixir2: bin
	@if command -v elixir >/dev/null; then \
	  elixir elixir/main2.exs > $(STACK_TRACE)/stack2.elixir.txt 2>&1 || true; \
	else echo "elixir not installed; skipping"; fi

erlang2: bin
	@if command -v escript >/dev/null; then \
	  escript erlang/main2.escript > $(STACK_TRACE)/stack2.erlang.txt 2>&1 || true; \
	else echo "escript not installed; skipping"; fi

fsharp2: | $(STACK_TRACE)
	@if command -v fsharpi >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  ( ulimit -c 0; fsharpi --exec fsharp/main2.fsx ) > "$(STACK_TRACE)/stack2.fsharp.txt" 2>&1 || true; \
	else echo "fsharpi not installed; skipping"; fi

go2: bin
	@if command -v go >/dev/null; then \
	  go build -o bin/go2 go/main2.go; \
	  ./bin/go2 > $(STACK_TRACE)/stack2.go.txt 2>&1 || true; \
	else echo "go not installed; skipping"; fi

haskell2: bin
	@if command -v ghc >/dev/null; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  if ghc -O0 -g -rtsopts -prof -fprof-auto -o bin/haskell2 haskell/Main2.hs >/dev/null 2>&1; then \
	    ./bin/haskell2 +RTS -xc -RTS > "$(STACK_TRACE)/stack2.haskell.txt" 2>&1 || true; \
	    rm -f haskell/Main2.o haskell/Main2.hi
	  else \
	    echo "ghc profiling libs not available; install with apt install ghc-prof but for now falling back to non-profiling build" >&2; \
	    ghc -O0 -g -rtsopts -o bin/haskell2 haskell/Main2.hs >/dev/null 2>&1 || true; \
	    ./bin/haskell2 > "$(STACK_TRACE)/stack2.haskell.txt" 2>&1 || true; \
	    rm -f haskell/Main2.o haskell/Main2.hi
	  fi; \
	else \
	  echo "ghc not installed; skipping"; \
	fi

java2: bin
	@if command -v javac >/dev/null; then \
	  mkdir -p bin/java; \
	  javac -g -d bin/java java/Main2.java; \
	  java -cp bin/java Main2 > $(STACK_TRACE)/stack2.java.txt 2>&1 || true; \
	else echo "javac not installed; skipping"; fi

javascript2: bin
	@if command -v node >/dev/null; then \
	  node javascript/main2.mjs > $(STACK_TRACE)/stack2.javascript.txt 2>&1 || true; \
	else echo "node not installed; skipping"; fi

julia2: bin
	@if command -v julia >/dev/null; then \
	  julia julia/main2.jl > $(STACK_TRACE)/stack2.julia.txt 2>&1 || true; \
	else echo "julia not installed; skipping"; fi

kotlin2: bin
	@if command -v kotlinc >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  $(FIND_JDK_SNIPPET); \
	  err=$$(mktemp); \
	  if [ -n "$$JDK_HOME" ]; then \
	    KOTLINC_JDK_FLAG="-jdk-home $$JDK_HOME"; \
	    JAVA_BIN="$$JDK_HOME/bin/java"; \
	  else \
	    KOTLINC_JDK_FLAG=""; \
	    JAVA_BIN="$$(command -v java || echo java)"; \
	  fi; \
	  # Compile
	  if ! kotlinc $$KOTLINC_JDK_FLAG kotlin/Main2.kt -include-runtime -d bin/kotlin2.jar 2> "$$err"; then \
	    if grep -Eq 'ASM9|Unsupported class file major version' "$$err"; then \
	      { echo "Kotlin compiler is too old for this JDK."; \
	        echo "Upgrade kotlinc (e.g. SDKMAN: 'sdk install kotlin 1.9.24' && 'sdk use kotlin 1.9.24')."; \
	      } > "$(STACK_TRACE)/stack2.kotlin.txt"; \
	    else \
	      cat "$$err" > "$(STACK_TRACE)/stack2.kotlin.txt"; \
	    fi; \
	    rm -f "$$err"; \
	    exit 0; \
	  fi; \
	  rm -f "$$err"; \
	  # Run
	  "$$JAVA_BIN" -cp bin/kotlin2.jar Main2 > "$(STACK_TRACE)/stack2.kotlin.txt" 2>&1 || true; \
	else \
	  echo "kotlinc not installed; skipping"; \
	fi

lua2: bin
	@if command -v lua >/dev/null; then \
	  lua lua/main2.lua > $(STACK_TRACE)/stack2.lua.txt 2>&1 || true; \
	else echo "lua not installed; skipping"; fi

nim2: bin
	@if command -v nim >/dev/null; then \
	  nim c -d:debug --out:bin/nim2 nim/main2.nim > /dev/null; \
	  ./bin/nim2 > $(STACK_TRACE)/stack2.nim.txt 2>&1 || true; \
	else echo "nim not installed; skipping"; fi

ocaml2: bin
	@if command -v ocamlopt >/dev/null; then \
	  ocamlopt -g -o bin/ocaml2 ocaml/main2.ml; \
	  OCAMLRUNPARAM=b ./bin/ocaml2 > $(STACK_TRACE)/stack2.ocaml.txt 2>&1 || true; \
	  rm -f ocaml/main2.cmi ocaml/main2.cmx ocaml/main2.o \
	else echo "ocamlopt not installed; skipping"; fi

odin2:
	@if command -v odin >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  odin build odin/main2.odin -file -out:bin/odin2 -debug; \
	  ( ulimit -c 0; ./bin/odin2 ) > "$(STACK_TRACE)/stack2.odin.txt" 2>&1 || true; \
	else echo "odin not installed; skipping"; fi


perl2: bin
	@if command -v perl >/dev/null; then \
	  perl perl/main2.pl > "$(STACK_TRACE)/stack2.perl.txt" 2>&1 || true; \
	else echo "perl not installed; skipping"; fi

php2: bin
	@if command -v php >/dev/null; then \
	  php php/main2.php > $(STACK_TRACE)/stack2.php.txt 2>&1 || true; \
	else echo "php not installed; skipping"; fi

python2: bin
	@if command -v python3 >/dev/null; then \
	  python3 -B python/main2.py > $(STACK_TRACE)/stack2.python.txt 2>&1 || true; \
	  rm -fr __pycache__		\ 
	else echo "python3 not installed; skipping"; fi

r2: bin
	@if command -v Rscript >/dev/null; then \
	  Rscript r/main2.R > $(STACK_TRACE)/stack2.r.txt 2>&1 || true; \
	else echo "Rscript not installed; skipping"; fi

ruby2: bin
	@if command -v ruby >/dev/null; then \
	  ruby ruby/main2.rb > $(STACK_TRACE)/stack2.ruby.txt 2>&1 || true; \
	else echo "ruby not installed; skipping"; fi

rust2: bin
	@if command -v rustc >/dev/null; then \
	  rustc -g -C debuginfo=2 -o bin/rust2 rust/main2.rs; \
	  RUST_BACKTRACE=1 ./bin/rust2 > $(STACK_TRACE)/stack2.rust.txt 2>&1 || true; \
	else echo "rustc not installed; skipping"; fi

scala2: bin
	@if command -v scalac >/dev/null; then \
	  scalac -g:vars -d bin/scala scala/Main2.scala; \
	  scala -cp bin/scala Main2 > $(STACK_TRACE)/stack2.scala.txt 2>&1 || true; \
	else echo "scalac not installed; skipping"; fi

smalltalk2: bin
	@if command -v gst >/dev/null; then \
	  gst smalltalk/main2.st > $(STACK_TRACE)/stack2.smalltalk.txt 2>&1 || true; \
	else echo "gnu smalltalk not installed; skipping"; fi

swift2: bin
	@if command -v swiftc >/dev/null; then \
	  swiftc -g swift/main2.swift -o bin/swift2; \
	  ./bin/swift2 > $(STACK_TRACE)/stack2.swift.txt 2>&1 || true; \
	else echo "swiftc not installed; skipping"; fi

v2: bin
	@if command -v v >/dev/null; then \
	  v -g -o bin/v2 v/main2.v; \
	  ./bin/v2 > $(STACK_TRACE)/stack2.v.txt 2>&1 || true; \
	else echo "v not installed; skipping"; fi

zig2: bin
	@if command -v zig >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  zig build-exe -O Debug -femit-bin=bin/zig2 zig/main2.zig; \
	  ( ulimit -c 0; ZIG_BACKTRACE=full ./bin/zig2 ) > "$(STACK_TRACE)/stack2.zig.txt" 2>&1 || true; \
	else echo "zig not installed; skipping"; fi

# ===================== Program III =====================

# ---------- Python ----------
python3:
	@mkdir -p $(BIN_DIR)/python >/dev/null 2>&1 || true
	@if command -v python3 >/dev/null 2>&1; then \
	  python3 -B python/main3.py > $(STACK_TRACE)/stack3.python.txt 2>&1 || true; \
	  rm -fr __pycache__		\ 
	else echo "[python3] SKIP: python3 not found"; fi

# ---------- Ada ----------
ada3:
	@mkdir -p $(BIN_DIR)/ada >/dev/null 2>&1 || true
	@if command -v gnatmake >/dev/null 2>&1; then \
	  gnatmake -g -o $(BIN_DIR)/ada/main3 ada/main3.adb > /dev/null 2>&1 && \
	  $(BIN_DIR)/ada/main3 > $(STACK_TRACE)/stack3.ada.txt 2>&1 || true; \
	  rm -f main3.ali main3.o main3.ali b~main3.o b~main3.ali b~main3 b~main3.adb b~main3.ads
	else echo "[ada] SKIP: gnatmake not found"; fi

# ---------- C# (.NET) ----------
csharp3: | $(STACK_TRACE)
	@if command -v docker >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  docker run --rm \
	    -u $$(id -u):$$(id -g) \
	    -e HOME=/tmp \
	    -e DOTNET_CLI_HOME=/tmp \
	    -e NUGET_PACKAGES=/tmp/nugetpackages \
	    -e DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1 \
	    -e DOTNET_CLI_TELEMETRY_OPTOUT=1 \
	    -e DOTNET_NOLOGO=1 \
	    -v "$$(pwd)/csharp3:/src" \
	    -v "$$(pwd)/$(STACK_TRACE):/out" \
	    -w /src \
	    mcr.microsoft.com/dotnet/sdk:8.0 \
	    bash -lc 'mkdir -p /tmp/nugetpackages; \
	              (dotnet restore >/dev/null 2>&1 || true); \
	              dotnet build -c Debug >/dev/null; \
	              ( ulimit -c 0; dotnet run -c Debug ) > /out/stack3.csharp.txt 2>&1' \
	    || true; \
		rm -rf csharp3/bin csharp3/obj \
	else echo "docker not installed; skipping C#"; fi

# ---------- Clean ----------
clean3:
	@if command -v clm >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  ( cd clean && clm -ns -ms -tst -h 100M -ci Main3 -o ../bin/clean3 ); \
	  ./bin/clean3 > "$(STACK_TRACE)/stack3.clean.txt" 2>&1 || true; \
	  rm -fr "clean/Clean System Files"
	else echo "Clean compiler (clm) not installed; skipping see https://clean.cs.ru.nl/Download_Clean"; fi

# ---------- Clojure ----------
clojure3: bin
	@if command -v clojure >/dev/null; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  errlog=$$(mktemp); outlog=$$(mktemp); \
	  ( cd clojure && clojure -e '(load-file "main3.clj")' ) > "$$outlog" 2> "$$errlog" || true; \
	  report=$$(awk '/^Full report at:/{getline; print $$0}' "$$errlog" | head -n1); \
	  if [ -n "$$report" ] && [ -f "$$report" ]; then \
	    cat "$$report" > "$(STACK_TRACE)/stack3.clojure.txt"; \
	  else \
	    cat "$$errlog" "$$outlog" > "$(STACK_TRACE)/stack3.clojure.txt"; \
	  fi; \
	  rm -f "$$errlog" "$$outlog"; \
	else \
	  echo "clojure not installed; skipping"; \
	fi

# ---------- Crystal ----------
crystal3:
	@mkdir -p $(BIN_DIR)/crystal >/dev/null 2>&1 || true
	@if command -v crystal >/dev/null 2>&1; then \
	  crystal build --debug -o $(BIN_DIR)/crystal/main3 crystal/main3.cr > /dev/null 2>&1 && \
	  $(BIN_DIR)/crystal/main3 > $(STACK_TRACE)/stack3.crystal.txt 2>&1 || true; \
	else echo "[crystal] SKIP: crystal not found"; fi

# ---------- D ----------
d3:
	@mkdir -p $(BIN_DIR)/d >/dev/null 2>&1 || true
	@if command -v dmd >/dev/null 2>&1; then \
	  dmd -g -of=$(BIN_DIR)/d/main3 d/main3.d > /dev/null 2>&1 && \
	  $(BIN_DIR)/d/main3 > $(STACK_TRACE)/stack3.d.txt 2>&1 || true; \
	elif command -v ldc2 >/dev/null 2>&1; then \
	  ldc2 -g -of=$(BIN_DIR)/d/main3 d/main3.d > /dev/null 2>&1 && \
	  $(BIN_DIR)/d/main3 > $(STACK_TRACE)/stack3.d.txt 2>&1 || true; \
	else echo "[d] SKIP: dmd/ldc2 not found"; fi

# ---------- Elixir ----------
elixir3:
	@mkdir -p $(BIN_DIR)/elixir >/dev/null 2>&1 || true
	@if command -v elixir >/dev/null 2>&1; then \
	  elixir elixir/main3.exs > $(STACK_TRACE)/stack3.elixir.txt 2>&1 || true; \
	else echo "[elixir] SKIP: elixir not found"; fi

# ---------- Erlang escript ----------
erlang3:
	@mkdir -p $(BIN_DIR)/erlang >/dev/null 2>&1 || true
	@if command -v escript >/dev/null 2>&1; then \
	  chmod +x erlang/main3.escript && escript erlang/main3.escript > $(STACK_TRACE)/stack3.erlang.txt 2>&1 || true; \
	else echo "[erlang] SKIP: escript not found"; fi

# ---------- F# (.NET) script ----------
fsharp3: | $(STACK_TRACE)
	@if command -v fsharpi >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  ( ulimit -c 0; fsharpi --exec fsharp/main3.fsx ) > "$(STACK_TRACE)/stack3.fsharp.txt" 2>&1 || true; \
	else echo "fsharpi not installed; skipping"; fi

# ---------- Go ----------
go3:
	@mkdir -p $(BIN_DIR)/go >/dev/null 2>&1 || true
	@if command -v go >/dev/null 2>&1; then \
	  go build -o $(BIN_DIR)/go/main3 go/main3.go > /dev/null 2>&1 && \
	  $(BIN_DIR)/go/main3 > $(STACK_TRACE)/stack3.go.txt 2>&1 || true; \
	else echo "[go] SKIP: go not found"; fi

# ---------- Haskell ----------
haskell3: bin
	@if command -v ghc >/dev/null; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  if ghc -O0 -g -rtsopts -prof -fprof-auto -o bin/haskell3 haskell/Main3.hs >/dev/null 2>&1; then \
	    ./bin/haskell3 +RTS -xc -RTS > "$(STACK_TRACE)/stack3.haskell.txt" 2>&1 || true; \
	    rm -f haskell/Main3.o haskell/Main3.hi
	  else \
	    echo "ghc profiling libs not available; install with apt install ghc-prof but for now falling back to non-profiling build" >&2; \
	    ghc -O0 -g -rtsopts -o bin/haskell3 haskell/Main3.hs >/dev/null 2>&1 || true; \
	    ./bin/haskell3 > "$(STACK_TRACE)/stack3.haskell.txt" 2>&1 || true; \
	    rm -f haskell/Main3.o haskell/Main3.hi
	  fi; \
	else \
	  echo "ghc not installed; skipping"; \
	fi

# ---------- Java ----------
java3:
	@mkdir -p $(BIN_DIR)/java >/dev/null 2>&1 || true
	@if command -v javac >/dev/null 2>&1; then \
	  javac -g -d $(BIN_DIR)/java java/Main3.java > /dev/null 2>&1 && \
	  java -cp $(BIN_DIR)/java Main3 > $(STACK_TRACE)/stack3.java.txt 2>&1 || true; \
	else echo "[java] SKIP: javac not found"; fi

# ---------- JavaScript (Node) ----------
javascript3:
	@mkdir -p $(BIN_DIR)/javascript >/dev/null 2>&1 || true
	@if command -v node >/dev/null 2>&1; then \
	  node javascript/main3.mjs > $(STACK_TRACE)/stack3.javascript.txt 2>&1 || true; \
	else echo "[javascript] SKIP: node not found"; fi

# ---------- Julia ----------
julia3:
	@mkdir -p $(BIN_DIR)/julia >/dev/null 2>&1 || true
	@if command -v julia >/dev/null 2>&1; then \
	  julia julia/main3.jl > $(STACK_TRACE)/stack3.julia.txt 2>&1 || true; \
	else echo "[julia] SKIP: julia not found"; fi

# ---------- Kotlin ----------
kotlin3:
	@mkdir -p $(BIN_DIR)/kotlin >/dev/null 2>&1 || true
	@if command -v kotlinc >/dev/null 2>&1; then \
	  kotlinc -include-runtime -d $(BIN_DIR)/kotlin/main3.jar kotlin/Main3.kt > /dev/null 2>&1 && \
	  java -jar $(BIN_DIR)/kotlin/main3.jar > $(STACK_TRACE)/stack3.kotlin.txt 2>&1 || true; \
	else echo "[kotlin] SKIP: kotlinc not found"; fi

# ---------- Lua ----------
lua3:
	@mkdir -p $(BIN_DIR)/lua >/dev/null 2>&1 || true
	@if command -v lua >/dev/null 2>&1; then \
	  lua lua/main3.lua > $(STACK_TRACE)/stack3.lua.txt 2>&1 || true; \
	else echo "[lua] SKIP: lua not found"; fi

# ---------- Nim ----------
nim3:
	@mkdir -p $(BIN_DIR)/nim >/dev/null 2>&1 || true
	@if command -v nim >/dev/null 2>&1; then \
	  nim c -d:debug --out:$(BIN_DIR)/nim/main3 nim/main3.nim > /dev/null 2>&1 && \
	  $(BIN_DIR)/nim/main3 > $(STACK_TRACE)/stack3.nim.txt 2>&1 || true; \
	else echo "[nim] SKIP: nim not found"; fi

# ---------- OCaml ----------
ocaml3:
	@mkdir -p $(BIN_DIR)/ocaml >/dev/null 2>&1 || true
	@if command -v ocamlopt >/dev/null 2>&1; then \
	  ocamlopt -g -o $(BIN_DIR)/ocaml/main3 ocaml/main3.ml > /dev/null 2>&1 && \
	  OCAMLRUNPARAM=b $(BIN_DIR)/ocaml/main3 > $(STACK_TRACE)/stack3.ocaml.txt 2>&1 || true; \
	  rm -f ocaml/main3.cmi ocaml/main3.cmx ocaml/main3.o \
	else echo "[ocaml] SKIP: ocamlopt not found"; fi

# ---------- Odin ----------
odin3:
	@if command -v odin >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  odin build odin/main3.odin -file -out:bin/odin3 -debug; \
	  ( ulimit -c 0; ./bin/odin3 ) > "$(STACK_TRACE)/stack3.odin.txt" 2>&1 || true; \
	else echo "odin not installed; skipping"; fi

# ---------- Perl ----------
perl3:
	@mkdir -p $(BIN_DIR)/perl >/dev/null 2>&1 || true
	@if command -v perl >/dev/null 2>&1; then \
	  perl perl/main3.pl > $(STACK_TRACE)/stack3.perl.txt 2>&1 || true; \
	else echo "[perl] SKIP: perl not found"; fi

# ---------- PHP ----------
php3:
	@mkdir -p $(BIN_DIR)/php >/dev/null 2>&1 || true
	@if command -v php >/dev/null 2>&1; then \
	  php php/main3.php > $(STACK_TRACE)/stack3.php.txt 2>&1 || true; \
	else echo "[php] SKIP: php not found"; fi

# ---------- R ----------
r3:
	@mkdir -p $(BIN_DIR)/r >/dev/null 2>&1 || true
	@if command -v Rscript >/dev/null 2>&1; then \
	  Rscript r/main3.R > $(STACK_TRACE)/stack3.r.txt 2>&1 || true; \
	else echo "[r] SKIP: Rscript not found"; fi

# ---------- Ruby ----------
ruby3:
	@mkdir -p $(BIN_DIR)/ruby >/dev/null 2>&1 || true
	@if command -v ruby >/dev/null 2>&1; then \
	  ruby ruby/main3.rb > $(STACK_TRACE)/stack3.ruby.txt 2>&1 || true; \
	else echo "[ruby] SKIP: ruby not found"; fi

# ---------- Rust ----------
rust3:
	@mkdir -p $(BIN_DIR)/rust >/dev/null 2>&1 || true
	@if command -v rustc >/dev/null 2>&1; then \
	  rustc -g -C debuginfo=2 -o $(BIN_DIR)/rust/main3 rust/main3.rs > /dev/null 2>&1 && \
	  RUST_BACKTRACE=1 $(BIN_DIR)/rust/main3 > $(STACK_TRACE)/stack3.rust.txt 2>&1 || true; \
	else echo "[rust] SKIP: rustc not found"; fi

# ---------- Scala ----------
scala3:
	@mkdir -p $(BIN_DIR)/scala >/dev/null 2>&1 || true
	@if command -v scalac >/dev/null 2>&1; then \
	  scalac -d $(BIN_DIR)/scala scala/Main3.scala > /dev/null 2>&1 && \
	  scala -cp $(BIN_DIR)/scala Main3 > $(STACK_TRACE)/stack3.scala.txt 2>&1 || true; \
	else echo "[scala] SKIP: scalac not found"; fi

# ---------- Smalltalk (GNU Smalltalk) ----------
smalltalk3:
	@mkdir -p $(BIN_DIR)/smalltalk >/dev/null 2>&1 || true
	@if command -v gst >/dev/null 2>&1; then \
	  gst smalltalk/main3.st > $(STACK_TRACE)/stack3.smalltalk.txt 2>&1 || true; \
	else echo "[smalltalk] SKIP: gst not found"; fi

# ---------- Swift ----------
swift3:
	@mkdir -p $(BIN_DIR)/swift >/dev/null 2>&1 || true
	@if command -v swiftc >/dev/null 2>&1; then \
	  swiftc -g -o $(BIN_DIR)/swift/main3 swift/main3.swift > /dev/null 2>&1 && \
	  $(BIN_DIR)/swift/main3 > $(STACK_TRACE)/stack3.swift.txt 2>&1 || true; \
	else echo "[swift] SKIP: swiftc not found"; fi

# ---------- V ----------
v3:
	@mkdir -p $(BIN_DIR)/v >/dev/null 2>&1 || true
	@if command -v v >/dev/null 2>&1; then \
	  v -g -o $(BIN_DIR)/v/main3 v/main3.v > /dev/null 2>&1 && \
	  $(BIN_DIR)/v/main3 > $(STACK_TRACE)/stack3.v.txt 2>&1 || true; \
	else echo "[v] SKIP: v compiler not found"; fi


# ---------- Zig ----------
zig3:
	@mkdir -p $(BIN_DIR)/zig >/dev/null 2>&1 || true
	@if command -v zig >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  zig build-exe -O Debug -femit-bin=bin/zig3 zig/main3.zig; \
	  ( ulimit -c 0; ZIG_BACKTRACE=full ./bin/zig3 ) > "$(STACK_TRACE)/stack3.zig.txt" 2>&1 || true; \
	else echo "[zig] SKIP: zig not found"; fi



# ===================== Program III =====================

# ---------- Python ----------
python4:
	@mkdir -p $(BIN_DIR)/python >/dev/null 2>&1 || true
	@if command -v python3 >/dev/null 2>&1; then \
	  python3 -B python/main4.py > $(STACK_TRACE)/stack4.python.txt 2>&1 || true; \
	  rm -fr __pycache__		\ 
	else echo "[python3] SKIP: python3 not found"; fi

# ---------- Ada ----------
ada4:
	@mkdir -p $(BIN_DIR)/ada >/dev/null 2>&1 || true
	@if command -v gnatmake >/dev/null 2>&1; then \
	  gnatmake -g -o $(BIN_DIR)/ada/main4 ada/main4.adb > /dev/null 2>&1 && \
	  $(BIN_DIR)/ada/main4 > $(STACK_TRACE)/stack4.ada.txt 2>&1 || true; \
	  rm -f main4.ali main4.o b~main4.adb b~main4.ali b~main4.o b~main4.ads ; \
	else echo "[ada] SKIP: gnatmake not found"; fi

# ---------- C# (.NET) ----------
csharp4: | $(STACK_TRACE)
	@if command -v docker >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  docker run --rm \
	    -u $$(id -u):$$(id -g) \
	    -e HOME=/tmp \
	    -e DOTNET_CLI_HOME=/tmp \
	    -e NUGET_PACKAGES=/tmp/nugetpackages \
	    -e DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1 \
	    -e DOTNET_CLI_TELEMETRY_OPTOUT=1 \
	    -e DOTNET_NOLOGO=1 \
	    -v "$$(pwd)/csharp4:/src" \
	    -v "$$(pwd)/$(STACK_TRACE):/out" \
	    -w /src \
	    mcr.microsoft.com/dotnet/sdk:8.0 \
	    bash -lc 'mkdir -p /tmp/nugetpackages; \
	              (dotnet restore >/dev/null 2>&1 || true); \
	              dotnet build -c Debug >/dev/null; \
	              ( ulimit -c 0; dotnet run -c Debug ) > /out/stack4.csharp.txt 2>&1' \
	    || true; \
		rm -rf csharp4/bin csharp4/obj \
	else echo "docker not installed; skipping C#"; fi

# ---------- Clean ----------
clean4:
	@if command -v clm >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  ( cd clean && clm -ms -tst -h 100M Main4 -o ../bin/clean4 ); \
	  ( ulimit -c 0; ./bin/clean4 ) > "$(STACK_TRACE)/stack4.clean.txt" 2>&1 || true; \
	  rm -fr "clean/Clean System Files"
	else echo "Clean driver (clm) not on PATH; export PATH=$$CLEAN_HOME/bin:$$CLEAN_HOME/exe:$$PATH"; fi


# ---------- Clojure ----------
clojure4: bin
	@if command -v clojure >/dev/null; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  errlog=$$(mktemp); outlog=$$(mktemp); \
	  ( cd clojure && clojure -e '(load-file "main4.clj")' ) > "$$outlog" 2> "$$errlog" || true; \
	  report=$$(awk '/^Full report at:/{getline; print $$0}' "$$errlog" | head -n1); \
	  if [ -n "$$report" ] && [ -f "$$report" ]; then \
	    cat "$$report" > "$(STACK_TRACE)/stack4.clojure.txt"; \
	  else \
	    cat "$$errlog" "$$outlog" > "$(STACK_TRACE)/stack4.clojure.txt"; \
	  fi; \
	  rm -f "$$errlog" "$$outlog"; \
	else \
	  echo "clojure not installed; skipping"; \
	fi

# ---------- Crystal ----------
crystal4:
	@mkdir -p $(BIN_DIR)/crystal >/dev/null 2>&1 || true
	@if command -v crystal >/dev/null 2>&1; then \
	  crystal build --debug -o $(BIN_DIR)/crystal/main4 crystal/main4.cr > /dev/null 2>&1 && \
	  $(BIN_DIR)/crystal/main4 > $(STACK_TRACE)/stack4.crystal.txt 2>&1 || true; \
	else echo "[crystal] SKIP: crystal not found"; fi

# ---------- D ----------
d4: bin
	@if command -v dmd >/dev/null 2>&1; then \
	  dmd -g -of=bin/d4 d/main4.d; \
	  bash -lc 'ulimit -c 0; exec >"$(STACK_TRACE)/stack4.d.txt" 2>&1; ./bin/d4; exit 0'; \
	else echo "dmd not installed; skipping"; fi

# ---------- Elixir ----------
elixir4:
	@mkdir -p $(BIN_DIR)/elixir >/dev/null 2>&1 || true
	@if command -v elixir >/dev/null 2>&1; then \
	  elixir elixir/main4.exs > $(STACK_TRACE)/stack4.elixir.txt 2>&1 || true; \
	else echo "[elixir] SKIP: elixir not found"; fi

# ---------- Erlang escript ----------
erlang4:
	@mkdir -p $(BIN_DIR)/erlang >/dev/null 2>&1 || true
	@if command -v escript >/dev/null 2>&1; then \
	  escript erlang/main4.escript > $(STACK_TRACE)/stack4.erlang.txt 2>&1 || true; \
	else echo "[erlang] SKIP: escript not found"; fi

# ---------- F# (.NET) script ----------
fsharp4: | $(STACK_TRACE)
	@if command -v fsharpi >/dev/null 2>&1; then \
	  mkdir -p "$(STACK_TRACE)"; \
	  ( ulimit -c 0; fsharpi --exec fsharp/main4.fsx ) > "$(STACK_TRACE)/stack4.fsharp.txt" 2>&1 || true; \
	else echo "fsharpi not installed; skipping"; fi

# ---------- Go ----------
go4:
	@mkdir -p $(BIN_DIR)/go >/dev/null 2>&1 || true
	@if command -v go >/dev/null 2>&1; then \
	  go build -o $(BIN_DIR)/go/main4 go/main4.go > /dev/null 2>&1 && \
	  $(BIN_DIR)/go/main4 > $(STACK_TRACE)/stack4.go.txt 2>&1 || true; \
	else echo "[go] SKIP: go not found"; fi

# ---------- Haskell ----------
haskell4: bin
	@if command -v ghc >/dev/null; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  if ghc -O0 -g -rtsopts -prof -fprof-auto -o bin/haskell4 haskell/Main4.hs >/dev/null 2>&1; then \
	    ./bin/haskell4 +RTS -xc -RTS > "$(STACK_TRACE)/stack4.haskell.txt" 2>&1 || true; \
	    rm -f haskell/Main4.o haskell/Main4.hi
	  else \
	    echo "ghc profiling libs not available; install with apt install ghc-prof but for now falling back to non-profiling build" >&2; \
	    ghc -O0 -g -rtsopts -o bin/haskell4 haskell/Main4.hs >/dev/null 2>&1 || true; \
	    ./bin/haskell4 > "$(STACK_TRACE)/stack4.haskell.txt" 2>&1 || true; \
	    rm -f haskell/Main4.o haskell/Main4.hi
	  fi; \
	else \
	  echo "ghc not installed; skipping"; \
	fi

# ---------- Java ----------
java4: bin
	@if command -v javac >/dev/null; then \
	  mkdir -p bin/java; \
	  javac -g -d bin/java java/Main4.java; \
	  java -cp bin/java Main4 > $(STACK_TRACE)/stack4.java.txt 2>&1 || true; \
	else echo "javac not installed; skipping"; fi


# ---------- JavaScript (Node) ----------
javascript4:
	@mkdir -p $(BIN_DIR)/javascript >/dev/null 2>&1 || true
	@if command -v node >/dev/null 2>&1; then \
	  node javascript/main4.mjs > $(STACK_TRACE)/stack4.javascript.txt 2>&1 || true; \
	else echo "[javascript] SKIP: node not found"; fi

# ---------- Julia ----------
julia4:
	@mkdir -p $(BIN_DIR)/julia >/dev/null 2>&1 || true
	@if command -v julia >/dev/null 2>&1; then \
	  julia julia/main4.jl > $(STACK_TRACE)/stack4.julia.txt 2>&1 || true; \
	else echo "[julia] SKIP: julia not found"; fi

# ---------- Kotlin ----------
kotlin4:
	@mkdir -p $(BIN_DIR)/kotlin >/dev/null 2>&1 || true
	@if command -v kotlinc >/dev/null 2>&1; then \
	  kotlinc -include-runtime -d $(BIN_DIR)/kotlin/main4.jar kotlin/Main4.kt > /dev/null 2>&1 && \
	  java -jar $(BIN_DIR)/kotlin/main4.jar > $(STACK_TRACE)/stack4.kotlin.txt 2>&1 || true; \
	else echo "[kotlin] SKIP: kotlinc not found"; fi

# ---------- Lua ----------
lua4:
	@mkdir -p $(BIN_DIR)/lua >/dev/null 2>&1 || true
	@if command -v lua >/dev/null 2>&1; then \
	  lua lua/main4.lua > $(STACK_TRACE)/stack4.lua.txt 2>&1 || true; \
	else echo "[lua] SKIP: lua not found"; fi

# ---------- Nim ----------
nim4:
	@mkdir -p $(BIN_DIR)/nim >/dev/null 2>&1 || true
	@if command -v nim >/dev/null 2>&1; then \
	  nim c -d:debug --out:$(BIN_DIR)/nim/main4 nim/main4.nim > /dev/null 2>&1 && \
	  $(BIN_DIR)/nim/main4 > $(STACK_TRACE)/stack4.nim.txt 2>&1 || true; \
	else echo "[nim] SKIP: nim not found"; fi

# ---------- OCaml ----------
ocaml4:
	@mkdir -p $(BIN_DIR)/ocaml >/dev/null 2>&1 || true
	@if command -v ocamlopt >/dev/null 2>&1; then \
	  ocamlopt -g -o $(BIN_DIR)/ocaml/main4 ocaml/main4.ml > /dev/null 2>&1 && \
	  OCAMLRUNPARAM=b $(BIN_DIR)/ocaml/main4 > $(STACK_TRACE)/stack4.ocaml.txt 2>&1 || true; \
	  rm -f ocaml/main4.cmi ocaml/main4.cmx ocaml/main4.o \
	else echo "[ocaml] SKIP: ocamlopt not found"; fi

# ---------- Odin ----------
odin4:
	@if command -v odin >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  odin build odin/main4.odin -file -out:bin/odin4 -debug; \
	  ( ulimit -c 0; ./bin/odin4 ) > "$(STACK_TRACE)/stack4.odin.txt" 2>&1 || true; \
	else echo "odin not installed; skipping"; fi



# ---------- Perl ----------
perl4:
	@mkdir -p $(BIN_DIR)/perl >/dev/null 2>&1 || true
	@if command -v perl >/dev/null 2>&1; then \
	  perl perl/main4.pl > $(STACK_TRACE)/stack4.perl.txt 2>&1 || true; \
	else echo "[perl] SKIP: perl not found"; fi

# ---------- PHP ----------
php4:
	@mkdir -p $(BIN_DIR)/php >/dev/null 2>&1 || true
	@if command -v php >/dev/null 2>&1; then \
	  php php/main4.php > $(STACK_TRACE)/stack4.php.txt 2>&1 || true; \
	else echo "[php] SKIP: php not found"; fi

# ---------- R ----------
r4:
	@mkdir -p $(BIN_DIR)/r >/dev/null 2>&1 || true
	@if command -v Rscript >/dev/null 2>&1; then \
	  Rscript r/main4.R > $(STACK_TRACE)/stack4.r.txt 2>&1 || true; \
	else echo "[r] SKIP: Rscript not found"; fi

# ---------- Ruby ----------
ruby4:
	@mkdir -p $(BIN_DIR)/ruby >/dev/null 2>&1 || true
	@if command -v ruby >/dev/null 2>&1; then \
	  ruby ruby/main4.rb > $(STACK_TRACE)/stack4.ruby.txt 2>&1 || true; \
	else echo "[ruby] SKIP: ruby not found"; fi

# ---------- Rust ----------
rust4:
	@mkdir -p $(BIN_DIR)/rust >/dev/null 2>&1 || true
	@if command -v rustc >/dev/null 2>&1; then \
	  rustc -g -C debuginfo=2 -o $(BIN_DIR)/rust/main4 rust/main4.rs > /dev/null 2>&1 && \
	  RUST_BACKTRACE=1 $(BIN_DIR)/rust/main4 > $(STACK_TRACE)/stack4.rust.txt 2>&1 || true; \
	else echo "[rust] SKIP: rustc not found"; fi

# ---------- Scala ----------
scala4:
	@mkdir -p $(BIN_DIR)/scala >/dev/null 2>&1 || true
	@if command -v scalac >/dev/null 2>&1; then \
	  scalac -d $(BIN_DIR)/scala scala/Main4.scala > /dev/null 2>&1 && \
	  scala -cp $(BIN_DIR)/scala Main4 > $(STACK_TRACE)/stack4.scala.txt 2>&1 || true; \
	else echo "[scala] SKIP: scalac not found"; fi

# ---------- Smalltalk (GNU Smalltalk) ----------
smalltalk4:
	@mkdir -p $(BIN_DIR)/smalltalk >/dev/null 2>&1 || true
	@if command -v gst >/dev/null 2>&1; then \
	  gst smalltalk/main4.st > $(STACK_TRACE)/stack4.smalltalk.txt 2>&1 || true; \
	else echo "[smalltalk] SKIP: gst not found"; fi

# ---------- Swift ----------
swift4:
	@mkdir -p $(BIN_DIR)/swift >/dev/null 2>&1 || true
	@if command -v swiftc >/dev/null 2>&1; then \
	  swiftc -g -o $(BIN_DIR)/swift/main4 swift/main4.swift > /dev/null 2>&1 && \
	  $(BIN_DIR)/swift/main4 > $(STACK_TRACE)/stack4.swift.txt 2>&1 || true; \
	else echo "[swift] SKIP: swiftc not found"; fi

# ---------- V ----------
v4:
	@mkdir -p $(BIN_DIR)/v >/dev/null 2>&1 || true
	@if command -v v >/dev/null 2>&1; then \
	  v -g -o $(BIN_DIR)/v/main4 v/main4.v > /dev/null 2>&1 && \
	  $(BIN_DIR)/v/main4 > $(STACK_TRACE)/stack4.v.txt 2>&1 || true; \
	else echo "[v] SKIP: v compiler not found"; fi

# ---------- Zig ----------
zig4:
	@mkdir -p $(BIN_DIR)/zig >/dev/null 2>&1 || true
	@if command -v zig >/dev/null 2>&1; then \
	  mkdir -p bin "$(STACK_TRACE)"; \
	  zig build-exe -O Debug -femit-bin=bin/zig4 zig/main4.zig; \
	  ( ulimit -c 0; ZIG_BACKTRACE=full ./bin/zig4 ) > "$(STACK_TRACE)/stack4.zig.txt" 2>&1 || true; \
	else echo "[zig] SKIP: zig not found"; fi

## All programs of a single langauge
ada:  ada1 ada2 ada3 ada4
clean: clean1 clean2 clean3 clean4
csharp: csharp1 csharp2 csharp3 csharp4
clojure: clojure1 clojure2 clojure3 clojure4
crystal: crystal1 crystal2 crystal3 crystal4
d: d1 d2 d3 d4
elixir: elixir1 elixir2 elixir3 elixir4
erlang: erlang1 erlang2 erlang3 erlang4
fsharp: fsharp1 fsharp2 fsharp3 fsharp4
go: go1 go2 go3 go4
haskell: haskell1 haskell2 haskell3 haskell4
java: java1 java2 java3 java4
javascript: javascript1 javascript2 javascript3 javascript4
julia: julia1 julia2 julia3 julia4
kotlin: kotlin1 kotlin2 kotlin3 kotlin4
lua: lua1 lua2 lua3 lua4
nim: nim1 nim2 nim3 nim4
ocaml: ocaml1 ocaml2 ocaml3 ocaml4
odin: odin1 odin2 odin3 odin4
perl: perl1 perl2 perl3 perl4
php: php1 php2 php3 php4
python: python1 python2 python3 python4
r: r1 r2 r3 r4
ruby: ruby1 ruby2 ruby3 ruby4
rust: rust1 rust2 rust3 rust4
scala: scala1 scala2 scala3 scala4
smalltalk: smalltalk1 smalltalk2 smalltalk3 smalltalk4
swift: swift1 swift2 swift3 swift4
v: v1 v2 v3 v4
zig: zig1 zig2 zig4

clear:
	@rm -rf bin	@1 	@2 
	@mkdir -p $(STACK_TRACE)
	@rm -f $(STACK_TRACE)/*.txt

versions:
	@if command -v gnatmake >/dev/null; then gnatmake --version | head -n1; else echo "Ada compiler (gnatmake) is not installed"; fi
	@if command -v csc >/dev/null; then echo "C# version $$(csc -version)"; else echo "C# compiler (csc) is not installed"; fi
	@if command -v clojure >/dev/null; then echo clojure version $$(clojure -e "(clojure-version)"); else echo "Clojure is not installed"; fi
	@if command -v crystal >/dev/null; then crystal --version | head -n1; else echo "Crystal is not installed"; fi
	@if command -v dmd >/dev/null; then dmd --version | head -n1; else echo "D compiler (dmd) is not installed"; fi
	@if command -v elixir >/dev/null; then elixir --version | sed -n '3p'; else echo "Elixir is not installed"; fi
	@if command -v erl >/dev/null; then echo erl version $$(erl -noshell -eval 'erlang:display(erlang:system_info(otp_release)), halt().'); else echo "Erlang (escript) is not installed"; fi
	@if command -v fsharpi >/dev/null; then fsharpi --help | head -n 1; else echo "F# is not installed"; fi
	@if command -v go >/dev/null; then go version; else echo "Go is not installed"; fi
	@if command -v ghc >/dev/null; then ghc --version; else echo "Haskell (ghc) is not installed"; fi
	@if command -v javac >/dev/null; then javac -version; else echo "Java compiler (javac) is not installed"; fi
	@if command -v node >/dev/null; then echo node $$(node --version); else echo "JavaScript runtime (node) is not installed"; fi
	@if command -v julia >/dev/null; then julia --version; else echo "Julia is not installed"; fi
	@if command -v kotlinc >/dev/null; then kotlinc -version; else echo "Kotlin is not installed"; fi
	@if command -v lua >/dev/null; then lua -v; else echo "Lua is not installed"; fi
	@if command -v nim >/dev/null; then nim --version | head -n1; else echo "Nim is not installed"; fi
	@if command -v ocamlc >/dev/null; then echo ocamlc version $$(ocamlc -version); else echo "OCaml is not installed"; fi
	@if command -v odin >/dev/null; then odin version; else echo "Odin is not installed"; fi
	@if command -v perl >/dev/null; then perl -v | head -n2 | tail -n1; else echo "Perl is not installed"; fi
	@if command -v php >/dev/null; then php --version | head -n1; else echo "PHP is not installed"; fi
	@if command -v python3 >/dev/null; then python3 --version; else echo "Python3 is not installed"; fi
	@if command -v R >/dev/null; then R --version | head -n1; else echo "R is not installed"; fi
	@if command -v ruby >/dev/null; then ruby --version; else echo "Ruby is not installed"; fi
	@if command -v rustc >/dev/null; then rustc --version; else echo "Rust is not installed"; fi
	@if command -v scalac >/dev/null; then scalac -version; else echo "Scala compiler (scalac) is not installed"; fi
	@if command -v gst >/dev/null; then gst --version | head -n1 ; else echo "Smalltalk (gst) is not installed"; fi
	@if command -v swiftc >/dev/null; then swiftc --version | head -n1; else echo "Swift compiler is not installed"; fi
	@if command -v v >/dev/null; then v version; else echo "V is not installed"; fi
	@if command -v zig >/dev/null; then echo zig version $$(zig version); else echo "Zig is not installed"; fi

