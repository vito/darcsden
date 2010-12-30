DARCSDEN_HOME = "/srv/darcs"
DARCSDEN_ROOT = "#{DARCSDEN_HOME}/.darcsden/darcsden"

LIMITS = { "darcsden" => 200.megabytes, "darcsden-ssh" => 100.megabytes }

["darcsden", "darcsden-ssh"].each { |exe|
  God.watch { |w|
    w.name = "#{exe}"
    w.uid = "darcsden"
    w.gid = "darcsden"
    w.interval = 30.seconds
    w.dir = DARCSDEN_ROOT
    w.start = "#{DARCSDEN_ROOT}/dist/build/#{exe}/#{exe}"
    w.log = "#{DARCSDEN_ROOT}/#{exe}.log"
    w.env = { "PATH" => "#{DARCSDEN_HOME}/.cabal/bin:" + ENV["PATH"] }

    w.start_if { |start|
      start.condition(:process_running) { |c|
        c.interval = 5.seconds
        c.running = false
      }
    }

    w.restart_if { |restart|
      restart.condition(:memory_usage) { |c|
        c.above = LIMITS[exe]
        c.times = [3, 5]
      }

      restart.condition(:cpu_usage) { |c|
        c.above = 50.percent
        c.times = 5
      }
    }

    w.lifecycle { |on|
      on.condition(:flapping) { |c|
        c.to_state = [:start, :restart]
        c.times = 5
        c.within = 5.minute
        c.transition = :unmonitored
        c.retry_in = 10.minutes
        c.retry_times = 5
        c.retry_within = 2.hours
      }
    }
  }
}
