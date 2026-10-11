#include<gtk/gtk.h>

// Merit-entry table (historically "constraint" table; now holds both
// objective terms ("operands") and constraints, distinguished by role).
#define CONSTRAINT_TYPE_ITEM (constraint_item_get_type())
G_DECLARE_FINAL_TYPE (ConstraintItem, constraint_item, CONSTRAINT, ITEM, GObject)

struct _ConstraintItem
{
   GObject parent_instance;
   const char *constraintName;
   double con;
   int role;      // ID_ROLE_OBJECTIVE=1 / ID_ROLE_CONSTRAINT=2 / 0 = blank row
   int conType;
   double targ;
   double weight; // objective terms only
   // Operand parameters (Par 1..5), as shown/typed: e.g. UCY's surface,
   // wavelength and zoom position.  "" = not used / the default.
   char *par[5];
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

static ConstraintItem * constraint_item_new(const char *constraintName,
    double con,
    int role,
    int conType,
    double targ,
    double weight,
    const char *par1, const char *par2, const char *par3,
    const char *par4, const char *par5)
{
   ConstraintItem  *item = g_object_new(CONSTRAINT_TYPE_ITEM, NULL);
   const char *pars[5] = {par1, par2, par3, par4, par5};
   for (int i = 0; i < 5; i++) item->par[i] = g_strdup(pars[i] ? pars[i] : "");
    // g_strdup (copy), NOT g_strup: g_strup uppercases IN PLACE and returns
    // the same pointer -- with a Fortran-owned, non-null-terminated buffer
    // that mutated caller memory and stored a pointer whose "string" ran into
    // the adjacent record fields (the AUTUI name-dropdown crash).
    item->constraintName = g_strdup(constraintName);
    item->con     = con;
    item->role    = role;
    item->conType = conType;
    item->targ    = targ;
    item->weight  = weight;
    return item;
}

GListModel * append_constraint_model(GListStore *store,
   const char *constraintName,
   double con,
   int role,
   int conType,
   double targ,
   double weight,
   const char *par1, const char *par2, const char *par3,
   const char *par4, const char *par5)
{

g_list_store_append(store, constraint_item_new(
    constraintName,
    con,
    role,
    conType,
    targ,
    weight,
    par1, par2, par3, par4, par5));
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

int constraint_item_get_role(ConstraintItem *item)
{
   return item->role;
}

double constraint_item_get_weight(ConstraintItem *item)
{
   return item->weight;
}

// One getter per parameter column (the Fortran column table takes a
// single-argument getter).
const char* constraint_item_get_par1(ConstraintItem *item) { return item->par[0]; }
const char* constraint_item_get_par2(ConstraintItem *item) { return item->par[1]; }
const char* constraint_item_get_par3(ConstraintItem *item) { return item->par[2]; }
const char* constraint_item_get_par4(ConstraintItem *item) { return item->par[3]; }
const char* constraint_item_get_par5(ConstraintItem *item) { return item->par[4]; }

// Total objective f = sum weight*(value-target)^2 over the operand rows, set
// from Fortran (buildConstraintTable) before the table binds so the
// contribution column can be shown as a percentage.
static double g_contribution_total = 0.0;
void set_contribution_total(double t) { g_contribution_total = t; }

// This operand's PERCENT share of the objective merit function
// f = sum weight*(value - target)^2.  Only objective ("operand") rows
// contribute (role==ID_ROLE_OBJECTIVE==1); constraints / blank rows return 0.
// The per-row percentages sum to 100.
double constraint_item_get_contribution(ConstraintItem *item)
{
   if (item->role != 1 || g_contribution_total <= 0.0) return 0.0;
   double d = item->con - item->targ;
   return 100.0 * item->weight * d * d / g_contribution_total;
}

GListModel * append_blank_constraint(GListStore *store)
{
   g_list_store_append(store, constraint_item_new(
      " ",
      0.0,
      0,
      1,
      0.0,
      1.0,
      "", "", "", "", ""));
   return G_LIST_MODEL(store);
}
