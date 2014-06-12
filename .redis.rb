require "redis"

def llen (key)
		Redis.new.llen key
end

def lget (key)
		r = Redis.new
		len = r.llen key
		r.lrange(key, 0, len)
end

if ARGV[0] == 'llen'
		p llen ARGV[1]
else
		p lget ARGV[1]
end

