(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                      People in the simulation
 *)

module G = Graphics ;;
open Config ;;
open Registry ;;
module Ctr = Counter ;;
module Viz = Visualization ;;
module Stat = Statistics ;;
module Utilities = Utilities ;;

(*....................................................................
                                People
 *)

class person (initx : int) (inity : int)
             (initstepsize : int)
             (initinfect : float) =
  object (self)
    val id : string = Utilities.gensym ()
    val mutable posx : int = initx
    val mutable posy : int = inity
    val mutable step_size : int = initstepsize
    val mutable infectiousness : float = initinfect

    method id : string = id
    method step_size : int = step_size
    method infectiousness : float = infectiousness

    method set_pos (x : int) (y : int) : unit =
      posx <- x;
      posy <- y
    method pos = posx, posy

    method set_step_size (new_step_size : int) : unit =
      step_size <- new_step_size

    method set_infectiousness (new_infect : float) : unit =
      infectiousness <- new_infect

    method move : unit =
      let x, y = self#pos in
      let newx, newy =
        Utilities.rand_step x y self#step_size in
      (* drop from old location in registry *)
      Registry.deregister (self :> thing_type);
      (* update location *)
      self#set_pos newx newy;
      (* re-add at the new location *)
      Registry.register (self :> thing_type)

    method update : unit =
      self#move

    method draw : unit =
      let x, y = self#pos in
      Viz.draw_circle x y G.black
  end ;;

(*....................................................................
                       People in various states

  Note that since these classes refer to each other, they must be
  simultaneously defined using `and` instead of sequentially defined
  as separate classes.
 *)

class susceptible (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_SUSCEPTIBLE
                   cINFECTIOUSNESS_SUSCEPTIBLE
            as super

    initializer
      Stat.susceptible#bump

    method! update =
      super#update;
      let posx, posy = self#pos in
      (* calculate total infectiousness of all neighbors *)
      let infectiousness_total =
        Utilities.sum_float
	  (List.map (fun obj -> obj#infectiousness)
                    (Registry.neighbors (self :> thing_type))) in
      (* if infected, update the registry by replacing this object
         with an infected one *)
      if Utilities.flip_coin infectiousness_total then
        begin
          Stat.susceptible#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new infected posx posy) :> thing_type)
        end
      (* if vaccinated, update the registry by replacing this object
         with an vaccinated one *)
      else if Utilities.flip_coin cVACCINATION_FREQ then
        begin
          Stat.susceptible#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new vaccinated posx posy) :> thing_type)
        end

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_SUSCEPTIBLE
  end

and (* class *) infected (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED
            as super

    initializer
      Stat.infected#bump

    val mutable time_left : int =
      let mean, deviation = cRECOVERY_PERIOD in
      int_of_float (Utilities.gaussian mean deviation)

    method! update =
      super#update;
      let posx, posy = self#pos in
      if time_left = 0 then
          begin
            Stat.infected#debump;
            Registry.deregister (self :> thing_type);
            if Utilities.flip_coin cMORTALITY then
              Registry.register ((new deceased posx posy) :> thing_type)
            else
              Registry.register ((new recovered posx posy) :> thing_type)
          end
      else
        time_left <- pred time_left

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_INFECTED;
      Viz.draw_circle ~size: (cNEIGHBOR_RADIUS * cPIXELS_PER_BLOCK)
                      ~filled: false
                      x y cCOLOR_INFECTED
  end

and (* class *) recovered (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_RECOVERED
                   cINFECTIOUSNESS_RECOVERED
            as super

    initializer
      Stat.recovered#bump

    val mutable time_left : int =
      let mean, deviation = cIMMUNITY_PERIOD in
      int_of_float (Utilities.gaussian mean deviation)

    method! update =
      super#update;
      let posx, posy = self#pos in
      if time_left = 0 then
        begin
          Stat.recovered#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new susceptible posx posy) :> thing_type)
        end
      else
        time_left <- pred time_left

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_RECOVERED
  end

and (* class *) deceased (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
        cSTEP_SIZE_DECEASED
        cINFECTIOUSNESS_DECEASED

    initializer
      Stat.deceased#bump

    method! update = ()

    method! draw =
      let x, y = self#pos in
      Viz.draw_cross x y cCOLOR_DECEASED
  end

and (* class *) vaccinated (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
        cSTEP_SIZE_VACCINATED
        cINFECTIOUSNESS_VACCINATED

    initializer
      Stat.vaccinated#bump

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_VACCINATED
  end
;;
