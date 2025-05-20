#include<gtk/gtk.h>

// Constraint table
#define CONSTRAINT_TYPE_ITEM (constraint_item_get_type())
G_DECLARE_FINAL_TYPE (ConstraintItem, constraint_item, CONSTRAINT, ITEM, GObject)

struct _ConstraintItem
{
   GObject parent_instance; 
   const char *constraintName;
   double con;
   int conType;
   double targ;
};

struct _ConstraintItemClass
{
   GObjectClass parent_class;
};

G_DEFINE_TYPE (ConstraintItem, constraint_item, G_TYPE_OBJECT)

static void constraint_item_init(ConstraintItem *item)
{
}

static void constraint_item_class_init(ConstraintItemClass *class)
{
}

const char *constraintName;
double con;
int conType;
double targ;

static ConstraintItem * constraint_item_new(const char *constraintName, 
    double con,
    int conType,
    double targ)
{
   ConstraintItem  *item = g_object_new(CONSTRAINT_TYPE_ITEM, NULL);
    item->constraintName = g_strup(constraintName);
    item->con     = con;
    item->conType = conType;
    item->targ    = targ;
    return item;
}

GListModel * append_constraint_model(GListStore *store, 
   const char *constraintName, 
   double con,
   int conType,
   double targ)
{

g_list_store_append(store, constraint_item_new(
    constraintName, 
    con,
    conType,
    targ));
return G_LIST_MODEL(store);
}

const char* constraint_item_get_name(ConstraintItem *item)
{
    return item->constraintName;
}

double constraint_item_get_value(ConstraintItem *item)
{
   return item->con;
}

double constraint_item_get_target(ConstraintItem *item)
{
   return item->targ;
}

int constraint_item_get_contype(ConstraintItem *item)
{
   return item->conType;
}

GListModel * append_blank_constraint(GListStore *store)
{
   g_list_store_append(store, constraint_item_new(
      " ", 
      0.0,
      1,
      0.0));
   return G_LIST_MODEL(store);
}