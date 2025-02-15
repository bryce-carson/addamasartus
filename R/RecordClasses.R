## Inheritance-based subclasses
setClass("ClassRecord", contains = "Record")
setClass("FactionRecord", contains = "Record")
setClass("MagicEffectRecord", contains = "Record")
setClass("ScriptRecord", contains = "Record")
setClass("DoorRecord", contains = "Record")
setClass("WeaponRecord", contains = "Record")
setClass("ContainerRecord", contains = "Record")
setClass("SpellRecord", contains = "Record")
setClass("CreatureRecord", contains = "Record")
setClass("NPCRecord", contains = "Record")
setClass("ArmorRecord", contains = "Record")
setClass("ClothingRecord", contains = "Record")
setClass("ActivatorRecord", contains = "Record")
setClass("AlchemyApparatusRecord", contains = "Record")
setClass("IngredientRecord", contains = "Record")
setClass("AlchemyRecord", contains = "Record")
setClass("DialogueTopicRecord", contains = "Record")

## Composition-based helper functions
createSimpleRecord <- function(name, flags = c(0, 0), subrecords = list()) {
  stopifnot(is.list(subrecords))
  stopifnot(all(vapply(subrecords, is, logical(length(subrecords)), "Subrecord")))

  size <- sum(as.integer(lapply(subrecords, function(d) d@size)))

  new("Record", name = name, flag1 = as.integer(flags[1]), flag2 = as.integer(flags[2]), size = size, data = subrecords)
}

## Example usage for composition-based records

tes3Record <- createSimpleRecord("TES3", data = list(Subrecord("HEDR", list())))
gmstRecord <- createSimpleRecord("GMST", data = list(Subrecord("NAME", list("GameSettingID"))))
