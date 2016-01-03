// {a: 1,b: 2, c: {d: 3, e: {f: 4}}} -> "a_1,b_2,c_d_3,c_e_f_4"

//写递归函数
//1. 函数的定义(参数有哪些，返回值是什么)
//2. 递归调用返回值,需要处理在返回吗？

 def hash_to_string(hash)
   hash.map do |k,v|
     if v.instance_of? Hash
       rr = hash_to_string(v)
       rr.split(",").map{|a| "#{k}_#{a}"}.join(",")
     else
       "#{k}_#{v}"
     end
   end.join(",")
 end