 #include<gtk/gtk.h>

// Operand table
#define OPERAND_TYPE_ITEM (operand_item_get_type())
G_DECLARE_FINAL_TYPE (OperandItem, operand_item, OPERAND, ITEM, GObject)

struct _OperandItem
{
   GObject parent_instance; 
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

//  OperandItem *item = operand_item_new(
//     operandName, 
//     waveLength,
//     fieldPos,
//     pupilX,
//     pupilY,
//     fieldX,
//     fieldY,
//     target,
//     currValue);   
// GListStore *store2 = g_list_store_new(G_TYPE_OBJECT);    
//g_list_store_append(store, item);
//g_list_store_append(store, store2);
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

const char* operand_item_get_name(OperandItem *item)
{
    return item->operandName;
}

int operand_item_get_wavelength(OperandItem *item)
{
   //return waveLength;
   return item->waveLength;
}

int operand_item_get_fieldpos(OperandItem *item)
{
   return item->fieldPos;
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