using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Data;
using System.ServiceModel;
using WCFServer;

public partial class _Default : System.Web.UI.Page
{
    //Connect WCF Service
    
    static EndpointAddress address;
    //static EndpointAddress address = new EndpointAddress("http://localhost:6000/service");
    static WSHttpBinding binding;
    static ChannelFactory<IService1> factory;
    IService1 proxy;


    enum ClientState
    {
        NotAvailable = 0,
        Started = 1,
        RequestingUpdate = 2,
        NoRequest = 3,
        VehicleUpdateSubmitted = 4,
        AddPathSubmitted = 5,
        AddVehicleSubmitted = 6
    } ;

    
    protected void Page_Load(object sender, EventArgs e)
    {
        address = new EndpointAddress("http://localhost:6000/service");
        binding = new WSHttpBinding();
        factory = new ChannelFactory<IService1>(binding, address);
        proxy = factory.CreateChannel();

        gvVehicle.AllowPaging = true;
        gvVehicle.PageSize = 10;
        gvVehicle.AllowSorting = true;
        //gvSignal.AllowPaging = true;
        //gvSignal.PageSize = 3;
        ViewState["SortExpression"] = "ID ASC";

        DataTable ntable = new DataTable();
        ntable.Columns.Add("ID");
        ntable.Columns.Add("NodeID");

        ntable.Columns["ID"].AutoIncrement = true;
        ntable.Columns["ID"].AutoIncrementSeed = 1;
        ntable.Columns["ID"].AutoIncrementStep = 1;

        DataColumn[] nkeys = new DataColumn[1];
        nkeys[0] = ntable.Columns["ID"];
        ntable.PrimaryKey = nkeys;

        ViewState["NTable"] = ntable;

    }

    //Read data from WCF Service
    protected void Button1_Click(object sender, EventArgs e)
    {
        //proxy.setNumberOfConnectedClients(1);
        gvVehicle.DataSource = null;
        gvVehicle.DataBind();

        DataTable table = new DataTable();
        table.Columns.Add("ID");
        table.Columns.Add("DriverType");
        table.Columns.Add("Speed");
        table.Columns.Add("TID");
        table.Columns.Add("Location");
        table.Columns.Add("Time");

        table.Columns["TID"].AutoIncrement = true;
        table.Columns["TID"].AutoIncrementSeed = 1;
        table.Columns["TID"].AutoIncrementStep = 1;

        DataColumn[] dcKeys = new DataColumn[1];
        dcKeys[0] = table.Columns["TID"];
        table.PrimaryKey = dcKeys;

        //int size = proxy.getServerVehicleDataSize();
        //Vehicle v;
        //Vehicle[] array = new Vehicle[size];
        //array = proxy.getServerVehicleData();

        int ID = 0;
        int DType = 0;
        float Speed = 0;
        float Location = 0;
        float Time = 0;
        //for (int i = 0; i < size; i++)
        //{
        //    v = array[i];
        //    ID = v.id;
        //    DType = v.drivertype;
        //    Speed = v.speed;
        //    Location = v.location;
        //    Time = v.timestep;
            
        //    table.Rows.Add(ID, DType, Speed, null, Location, Time);
        //}


        DataView dvVehicle = new DataView(table);

        ViewState["table"] = table;
        BindingGridView();
    }

    private void BindingGridView()
    {
        if (ViewState["table"] != null)
        {

            DataTable table = (DataTable)ViewState["table"];

            DataView dv = new DataView(table);

            dv.Sort = ViewState["SortExpression"].ToString();

            // Bind the GridView control.
            gvVehicle.DataSource = dv;
            gvVehicle.DataBind();
        }
    }


    protected void AddRow_Click(object sender, EventArgs e)
    {
        /*DataTable table = (DataTable)ViewState["table"];
        DataRow newRow = table.NewRow();
        newRow["ID"] = TextBoxID.Text.ToString();
        newRow["DriverType"] = TextBoxDType.Text.ToString();
        newRow["Speed"] = TextBoxSpeed.Text.ToString();
        table.Rows.Add(newRow);
        BindingGridView();*/
        NewVehicle[] newVehs = new NewVehicle[1];
        
        //newVehs[0].time = proxy.getServerTimestep() + 1;
        String inode = TextBoxNID.Text.ToString();
        String pathid = TextBoxPID.Text.ToString();
        String driver = TextBoxDType.Text.ToString();
        String fleet = TextBoxFleet.Text.ToString(); 
        String vtype = TextBoxVType.Text.ToString(); 
        String overspeed = TextBoxOspeed.Text.ToString();  
        String range = TextBoxRange.Text.ToString();
        newVehs[0].inode = Convert.ToInt32(inode);
        newVehs[0].pathid = Convert.ToInt32(pathid);
        newVehs[0].driver = Convert.ToInt32(driver);
        newVehs[0].fleet = Convert.ToInt32(fleet);
        newVehs[0].vtype = Convert.ToInt32(vtype);
        newVehs[0].overspeed = Convert.ToInt32(overspeed);
        newVehs[0].range = Convert.ToInt32(range);

        //proxy.setServerNewVehicleData(newVehs);
        //proxy.setClientState(6); 
    }



    protected void Submit_Click(object sender, EventArgs e)
    {
        int size = gvVehicle.Rows.Count;
        //Vehicle[] vehs = new Vehicle[size];
        //proxy.setServerVehicleDataSize(size);

        DataTable table = (DataTable)ViewState["table"];

        //for (int i = 0; i < size; i++)
        //{
        //    String id = ((Label)gvVehicle.Rows[i].FindControl("Label1")).Text;
        //    String drivertype = ((Label)gvVehicle.Rows[i].FindControl("Label2")).Text;
        //    String speed = ((Label)gvVehicle.Rows[i].FindControl("Label3")).Text;
        //    String Location = ((Label)gvVehicle.Rows[i].FindControl("Label4")).Text;
        //    //String Time = ((Label)gvVehicle.Rows[i].FindControl("Label5")).Text;
        //    vehs[i].id = Convert.ToInt32(id);
        //    vehs[i].drivertype = Convert.ToInt32(drivertype);
        //    vehs[i].speed = Convert.ToSingle(speed);
        //    vehs[i].location = Convert.ToSingle(Location);
        //    //vehs[i].timestep = Convert.ToSingle(Time);
        //}

        //proxy.setServerVehicleData(vehs);       
        //proxy.setClientState(4);
        SubmitLabel.InnerText = "Submit Successfully!";
    }


    protected void gvVehicle_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        // Set the index of the new display page.  
        gvVehicle.PageIndex = e.NewPageIndex;

        // Rebind the GridView control to 
        // show data in the new page.
        BindingGridView();
    }

    protected void gvVehicle_RowDataBound(object sender, GridViewRowEventArgs e)
    {
        // Make sure the current GridViewRow is a data row.
        if (e.Row.RowType == DataControlRowType.DataRow)
        {
            // Make sure the current GridViewRow is either 
            // in the normal state or an alternate row.
            if (e.Row.RowState == DataControlRowState.Normal || e.Row.RowState == DataControlRowState.Alternate)
            {
                // Add client-side confirmation when deleting.
                ((LinkButton)e.Row.Cells[1].Controls[0]).Attributes["onclick"] = "if(!confirm('Are you certain you want to delete this one ?')) return false;";
            }
        }
    }

    protected void gvVehicle_RowEditing(object sender, GridViewEditEventArgs e)
    {
        // Make the GridView control into edit mode 
        // for the selected row. 
        gvVehicle.EditIndex = e.NewEditIndex;

        // Rebind the GridView control to show data in edit mode.
        BindingGridView();
    }

    protected void gvVehicle_RowCancelingEdit(object sender, GridViewCancelEditEventArgs e)
    {
        // Exit edit mode.
        gvVehicle.EditIndex = -1;

        // Rebind the GridView control to show data in view mode.
        BindingGridView();
    }

    // GridView.RowUpdating Event
    protected void gvVehicle_RowUpdating(object sender, GridViewUpdateEventArgs e)
    {
        if (ViewState["table"] != null)
        {
            // Get the DataTable from ViewState.
            DataTable dt = (DataTable)ViewState["table"];

            // Get the ID of the selected row.
            string strID = gvVehicle.Rows[e.RowIndex].Cells[2].Text;

            // Find the row in DateTable.
            DataRow dr = dt.Rows.Find(strID);

            // Retrieve edited values and updating respective items.

            dr["ID"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox1")).Text;
            dr["DriverType"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox2")).Text;
            dr["Speed"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox3")).Text;
            dr["Location"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox4")).Text;
            dr["Time"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox5")).Text;
            // Exit edit mode.
            gvVehicle.EditIndex = -1;

            // Rebind the GridView control to show data after updating.
            BindingGridView();

        }
    }

    protected void gvVehicle_RowDeleting(object sender, GridViewDeleteEventArgs e)
    {
        if (ViewState["table"] != null)
        {
            // Get the DataTable from ViewState.
            DataTable dt = (DataTable)ViewState["table"];

            // Get the ID of the selected row.
            string strID = gvVehicle.Rows[e.RowIndex].Cells[2].Text;

            // Find the row in DateTable.
            DataRow dr = dt.Rows.Find(strID);

            // Remove the row from the DataTable.
            dt.Rows.Remove(dr);

            // Rebind the GridView control to show data after deleting.
            BindingGridView();
        }
    }

    protected void gvVehicle_Sorting(object sender, GridViewSortEventArgs e)
    {
        string[] strSortExpression = ViewState["SortExpression"].ToString().Split(' ');

        // If the sorting column is the same as the previous one, 
        // then change the sort order.
        if (strSortExpression[0] == e.SortExpression)
        {
            if (strSortExpression[1] == "ASC")
            {
                ViewState["SortExpression"] = e.SortExpression + " " + "DESC";
            }
            else
            {
                ViewState["SortExpression"] = e.SortExpression + " " + "ASC";
            }
        }
        // If sorting column is another column, 
        // then specify the sort order to "Ascending".
        else
        {
            ViewState["SortExpression"] = e.SortExpression + " " + "ASC";
        }

        // Rebind the GridView control to show sorted data.
        BindingGridView();
    }


    protected void Button2_Click(object sender, EventArgs e)
    {
        //proxy.setClientState(2);
    }

    protected void End_Click(object sender, EventArgs e)
    {
       // proxy.setClientState(3);
    }


    protected void AddPath_Click(object sender, EventArgs e)
    {
        int size = gvPath.Rows.Count;
        int[] nodes = new int[size];
        String nodeid;
        for (int i = 0; i < size; i++)
        {
            nodeid = ((Label)gvPath.Rows[i].FindControl("LabelNode")).Text;
            nodes[i] = Convert.ToInt32(nodeid);
        }
        //proxy.setServerPathData(nodes);
       // proxy.setClientState(5); 
    }

    protected void AddNode_Click(object sender, EventArgs e)
    {
        DataTable nt = (DataTable)ViewState["NTable"];
        for (int i = 0; i < gvPath.Rows.Count; i++)
        {
            String nid = ((Label)gvPath.Rows[i].FindControl("LabelNode")).Text;
            nt.Rows.Add(null, nid);
        }

        nt.Rows.Add(null, TextBoxNodeID.Text);
        gvPath.DataSource = nt;
        gvPath.DataBind();
    }
}
