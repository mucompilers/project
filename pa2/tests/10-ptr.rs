fn main() {
   let mut x : i32 = 5;
   let mut b : bool = true;

   let y : &i32 = &7;
   let c : &bool = &false;

   let z : & &i32 = &y;
   let d : & &bool = &c;

   x = **z + **z;
   b = **d || (**z > 3);

   let p : Box<&mut i32> = Box::new(&mut x);
   let q : i32 = **p;
}
