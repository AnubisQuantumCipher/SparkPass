pragma SPARK_Mode (On);

with Interfaces; use type Interfaces.Unsigned_8;

package SparkPass.Runtime is
   -- High-assurance runtime toggle (process and persisted)
   -- Default comes from Config.High_Assurance_Mode; persisted value overrides.

   function High_Assurance_Enabled return Boolean
     with Global => null;

   procedure Set_High_Assurance (Enabled : Boolean)
     with Global => null;

end SparkPass.Runtime;

