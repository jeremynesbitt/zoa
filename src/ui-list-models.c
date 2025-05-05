 /*
  * This file contains the models for the ui tables
  * This could be done in fortran, but since it is a pain to 
  * create and access the models try this out
  */

 #include<gtk/gtk.h>

 #define LENS_TYPE_ITEM (lens_item_get_type())
 G_DECLARE_FINAL_TYPE (LensItem, lens_item, CAPITAL, ITEM, GObject)

 struct _LensItem
 {
     GObject parent_instance;
     int surfaceNo;
     bool refSurf;
     const char *surfaceName;
     const char *surfaceType;
     double radius;
     int radiusMod;
     double thickness ;
     int thickMod;
     const char *glass;
     double aperture;
     double index;
     double extraParams[16]; // Array of 16 elements

 };

 struct _LensItemClass
 {
     GObjectClass parent_class;
 };

 G_DEFINE_TYPE (LensItem, lens_item, G_TYPE_OBJECT)

 static void lens_item_init(LensItem *item)
 {
 }

 static void lens_item_class_init(LensItemClass *class)
 {
 }

 static LensItem * lens_item_new(int surfaceNo, 
                                       bool refSurf,
                                       const char *surfaceName,
                                       const char *surfaceType,
                                       double radius,
                                       int radiusMod,
                                       double thickness,
                                       int thickMod,
                                       const char *glass,
                                       double aperture,
                                       double index, 
                                       double extraParams[16])
 {
    LensItem  *item = g_object_new(LENS_TYPE_ITEM, NULL);
     item->surfaceNo = surfaceNo;
     item->refSurf   = refSurf;
     item->surfaceName = g_strdup(surfaceName);
     item->surfaceType = g_strdup(surfaceType);
     item->radius      = radius;
     item->radiusMod   = radiusMod;
     item->thickness   = thickness;
     item->thickMod    = thickMod;
     item->glass = g_strdup(glass);
     item->aperture = aperture;
     item->index = index;
     for(int i = 0; i < 15; i++){
      item->extraParams[i] = extraParams[i];
     }    
     //item->extraParams = extraParams;
     
     return item;
 }

 // a funcion that creates a GListModel with capital_item  objects
 GListModel * append_lens_model(GListStore *store, int surfaceNo, 
                                       bool refSurf,
                                       const char *surfaceName,
                                       const char *surfaceType,
                                       double radius,
                                       int radiusMod,
                                       double thickness,
                                       int thickMod,
                                       const char *glass,
                                       double aperture,
                                       double index, 
                                       double extraParams[16])
 {
     //GListStore *store = g_list_store_new(G_TYPE_OBJECT);
     g_list_store_append(store, lens_item_new(surfaceNo, 
                                              refSurf,
                                              surfaceName,
                                              surfaceType,
                                              radius,
                                              radiusMod,
                                              thickness,
                                              thickMod,
                                              glass,
                                              aperture,
                                              index, 
                                              extraParams));
     return G_LIST_MODEL(store);
 }

 int lens_item_get_surface_number(LensItem *item)
 {
    return item->surfaceNo;
 }

 bool lens_item_get_ref_surf(LensItem *item)
 {
    return item->refSurf;
 }


 const char* lens_item_get_surface_name(LensItem *item)
 {
     return item->surfaceName;
 }

 const char* lens_item_get_surface_type(LensItem *item)
 {
     return item->surfaceType;
 }

 double lens_item_get_surface_radius(LensItem *item)
 {
    return item->radius;
 }

 const char* lens_item_get_glass(LensItem *item)
 {
     return item->glass;
 }

 int lens_item_get_radius_mod(LensItem *item)
 {
    return item->radiusMod;
 }

 double lens_item_get_surface_thickness(LensItem *item)
 {
    return item->thickness;
 }

  int lens_item_get_thickness_mod(LensItem *item)
 {
    return item->thickMod;
 }

  double lens_item_get_aperture(LensItem *item)
 {
    return item->aperture;
 }

  double lens_item_get_extra_param(LensItem *item, int index)
 {
    return item->extraParams[index];
 }


// Operand table
#define OPERAND_TYPE_ITEM (operand_item_get_type())
G_DECLARE_FINAL_TYPE (OperandItem, operand_item, OPERAND, ITEM, GObject)

struct _OperandItem
{
   const char *operandName;
   int waveLength;
   int fieldPos;
   double pupilX;
   double pupilY;
   double fieldX;
   double fieldY;
   double target;
   double currValue;
};

struct _OperandItemClass
{
   GObjectClass parent_class;
};

G_DEFINE_TYPE (OperandItem, operand_item, G_TYPE_OBJECT)

static void operand_item_init(OperandItem *item)
{
}

static void operand_item_class_init(OperandItemClass *class)
{
}

const char *operandName;
int waveLength;
int fieldPos;
double pupilX;
double pupilY;
double fieldX;
double fieldY;
double target;
double currValue;

static OperandItem * operand_item_new(const char *operandName, 
   int waveLength,
   int fieldPos,
   double pupilX,
   double pupilY,
   double fieldX,
   double fieldY,
   double target,
   double currValue)
{
   OperandItem  *item = g_object_new(OPERAND_TYPE_ITEM, NULL);
    item->operandName = g_strup(operandName);
    item->waveLength   = waveLength;
    item->fieldPos       = fieldPos;
    item->pupilX       = pupilX;
    item->pupilY       = pupilY;
    item->fieldX       = fieldX;
    item->fieldY       = fieldY;
    item->target       = target;
    item->currValue = currValue;
    return item;
}

GListModel * append_operand_model(GListStore *store, 
   const char *operandName, 
   int waveLength,
   int fieldPos,
   double pupilX,
   double pupilY,
   double fieldX,
   double fieldY,
   double target,
   double currValue)
{
//GListStore *store = g_list_store_new(G_TYPE_OBJECT);
g_list_store_append(store, operand_item_new(
   operandName, 
   waveLength,
   fieldPos,
   pupilX,
   pupilY,
   fieldX,
   fieldY,
   target,
   currValue));
return G_LIST_MODEL(store);
}

GListModel * append_blank_operand(GListStore *store)
{
   g_list_store_append(store, operand_item_new(
      " ", 
      0,
      0,
      0.0,
      0.0,
      0.0,
      0.0,
      0.0,
      0.0));
   return G_LIST_MODEL(store);
}

