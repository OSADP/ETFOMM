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
        NotAvailable,
        Started,
        RequestingUpdate,
        NoRequest,
        APIShutDown,
        VehicleUpdateSubmitted,
        AddPathSubmitted,
        AddVehicleSubmitted,
        SignalsDataSubmitted,
        FixTimeControlSignalsDataSubmitted,
        Continued,
        Finished,
        UseHostDefineNetwork,
        UseClientDefineNetwork,
        UseTRFFile,
        NextRun,
        UpdateAll,
        UpdateFVehicle,
        UpdateSVehicle,
        UpdateFreewayLink,
        UpdateStreetLink,
        UpdateFTCSignal,
        UpdateACSignal,
        UpdateEntryNode,
        UpdateRampMeter,
        UpdateNInputs,
        UpdateFNInputs,
        UpdateSNInputs,
        UpdateVTypes,
        UpdateFDetInputs,
        UpdateSDetInputs,
        UpdateCPTInputs,
        UpdateBRInputs,
        UpdateBSInputs,
        UpdateIncidents,
        UpdateXYCoords,
        UpdateParkZInputs,
        UpdateEventInputs,
        UpdateDiverInputs,
        DataReady,
        GetCoordinationData,
        GotCoordinationData,
        SetCoordinationData,
        SetCoordinationDataDone,
        SetNewCycleLength,
        GetNewCycleLength,
        GetCycleLength,
        SetNewOffset,
        GetNewOffset,
        GetOffset,
        GetLocalCycleTimer,
        SetNewSplits,
        GetNewSplits,
        GetSplits,
        GetMinSplits,
        GetAverageTravelTime,
        GotAverageTravelTime,
        GetStreetLaneMOEData,
        GotStreetLaneMOEData,
        GetStreetLinkMOEData,
        GotStreetLinkMOEData,
        GetNodeGYR,
        SetNodeGYR
    };

    
    protected void Page_Load(object sender, EventArgs e)
    {
        address = new EndpointAddress("http://localhost:8000/service");
        binding = new WSHttpBinding();
        binding.MaxBufferPoolSize = 2147483647;
        binding.MaxReceivedMessageSize = 2147483647; 
        binding.Security.Mode = SecurityMode.None;
        factory = new ChannelFactory<IService1>(binding, address);
        proxy = factory.CreateChannel();

        gvVehicle.AllowPaging = true;
        gvVehicle.PageSize = 10;
        gvVehicle.AllowSorting = true;
        //gvSignal.AllowPaging = true;
        //gvSignal.PageSize = 3;
        ViewState["SortExpression"] = "ID ASC";
    }

    //Read data from WCF Service
    protected void Button1_Click(object sender, EventArgs e)
    {
        ShowVehicles();
    }

    private void ShowVehicles()
    {
        gvVehicle.DataSource = null;
        gvVehicle.DataBind();

        DataTable table = new DataTable();
        table.Columns.Add("ID");
        table.Columns.Add("DriverType");
        table.Columns.Add("Speed");
        table.Columns.Add("Location");
        table.Columns.Add("Lane");
        table.Columns.Add("Link");
        table.Columns.Add("TID");

        table.Columns["TID"].AutoIncrement = true;
        table.Columns["TID"].AutoIncrementSeed = 1;
        table.Columns["TID"].AutoIncrementStep = 1;

        DataColumn[] dcKeys = new DataColumn[1];
        //dcKeys[0] = table.Columns["TID"];
        // use vehicle ID as the table key
        dcKeys[0] = table.Columns["ID"]; 
        table.PrimaryKey = dcKeys;

        int size = proxy.GetServerFVehicleDataSize();

        WCF_VFData v;
        WCF_VFData[] array = new WCF_VFData[size];
        array = proxy.GetServerFVehicleData();

        int ID = 0;
        int DType = 0;
        float Speed = 0;
        float Location = 0;
        int Lane = 0;
        int Link = 0;
        int nValidVFData = 0;
        for (int i = 0; i < size; i++)
        {
            v = array[i];
            if (v.id == 0)
                continue;
            nValidVFData++;
            ID = v.id;
            DType = v.drivertype;
            Speed = v.speed;
            Location = v.location;
            Lane = v.lane;
            Link = v.link;

            table.Rows.Add(ID, DType, Speed, Location, Lane, Link, null);
        }

        TextBoxVehicleSize.Text = nValidVFData.ToString();

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
        string strNID = DropDownListNodeID.SelectedItem.Text;

        NewVehicle[] newVehs = new NewVehicle[1];

        String pathid = TextBoxPID.Text.ToString();
        String driver = TextBoxDType.Text.ToString();
        String fleet = TextBoxFleet.Text.ToString(); 
        String vtype = TextBoxVType.Text.ToString(); 
        String overspeed = TextBoxOspeed.Text.ToString();  
        String range = TextBoxRange.Text.ToString();
        newVehs[0].inode = Convert.ToInt32(strNID);
        newVehs[0].pathid = Convert.ToInt32(pathid);
        newVehs[0].driver = Convert.ToInt32(driver);
        newVehs[0].fleet = Convert.ToInt32(fleet);
        newVehs[0].vtype = Convert.ToInt32(vtype);
        newVehs[0].overspeed = Convert.ToInt32(overspeed);
        newVehs[0].range = Convert.ToInt32(range);

        proxy.SetServerNewVehicleData(newVehs);
        proxy.SetClientState((int)ClientState.AddVehicleSubmitted);

        ShowNextTimeStep();
        ShowVehicles();
    }



    protected void Submit_Click(object sender, EventArgs e)
    {
        if (ViewState["table"] != null)
        {
            int size = proxy.GetServerFVehicleDataSize();
            WCF_VFData[] vehs = new WCF_VFData[size];
            vehs = proxy.GetServerFVehicleData();
            // Get the DataTable from ViewState.
            DataTable dt = (DataTable)ViewState["table"];
            
            for (int i = 0; i < size; ++i)
            {
                if (vehs[i].id == 0) continue;
                string strID = vehs[i].id.ToString();
                DataRow dr = dt.Rows.Find(strID);
                
                vehs[i].drivertype = Convert.ToInt32(dr["DriverType"]);
                vehs[i].speed = Convert.ToSingle(dr["Speed"]);
                vehs[i].location = Convert.ToSingle(dr["Location"]);
                vehs[i].lane = Convert.ToInt32(dr["Lane"]);
                vehs[i].link = Convert.ToInt32(dr["Link"]);
            }
            proxy.SetServerFVehicleData(vehs);
            proxy.SetClientState((int)ClientState.VehicleUpdateSubmitted);
            ShowNextTimeStep();
            ShowVehicles();
        }
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
            //string strID = gvVehicle.Rows[e.RowIndex].Cells[3].Text;
            string strID = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox1")).Text; ;

            // Find the row in DateTable.
            DataRow dr = dt.Rows.Find(strID);

            // Retrieve edited values and updating respective items.

            dr["ID"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox1")).Text;
            dr["DriverType"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox2")).Text;
            dr["Speed"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox3")).Text;
            dr["Location"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox4")).Text;
            dr["Lane"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox5")).Text;
            dr["Link"] = ((TextBox)gvVehicle.Rows[e.RowIndex].FindControl("TextBox6")).Text;
            
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

    protected void End_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.NoRequest);
    }

    protected void TimeButton_Click(object sender, EventArgs e)
    {
        String timeinterval = TextBoxTimestep.Text;
        int ti = Convert.ToInt32(timeinterval);
        proxy.SetAPITimestepInterval(ti);
    }

    protected void UpdateAllButton_Click(object sender, EventArgs e)
    {
        String column = DropDownList1.SelectedItem.Value;
        DataTable dt = (DataTable)ViewState["table"];

        dt.Rows.Clear();

        dt.Columns["TID"].AutoIncrementSeed = -1;
        dt.Columns["TID"].AutoIncrementStep = -1;
        dt.Columns["TID"].AutoIncrementSeed = 1;
        dt.Columns["TID"].AutoIncrementStep = 1;



        int size = gvVehicle.Rows.Count;
        for (int i = 0; i < size; i++){
            String id = ((Label)gvVehicle.Rows[i].FindControl("Label1")).Text;
            String drivertype = ((Label)gvVehicle.Rows[i].FindControl("Label2")).Text;
            String speed = ((Label)gvVehicle.Rows[i].FindControl("Label3")).Text;
            String Location = ((Label)gvVehicle.Rows[i].FindControl("Label4")).Text;
            String Lane = ((Label)gvVehicle.Rows[i].FindControl("Label5")).Text;
            String Link = ((Label)gvVehicle.Rows[i].FindControl("Label6")).Text;
            if (Convert.ToInt32(id) != 0)
            {
                switch (column)
                {
                    case "DriverType":
                        dt.Rows.Add(id, TextBoxUpdateAll.Text, speed, Location, Lane, Link, null);
                        break;

                    case "Speed":
                        dt.Rows.Add(id, drivertype, TextBoxUpdateAll.Text, Location, Lane, Link, null);
                        break;

                    case "Location":
                        dt.Rows.Add(id, drivertype, speed, TextBoxUpdateAll.Text, Lane, Link, null);
                        break;

                    case "Lane":
                        dt.Rows.Add(id, drivertype, speed, Location, TextBoxUpdateAll.Text, Link, null);
                        break;
                    case "Link":
                        dt.Rows.Add(id, drivertype, speed, Location, Lane, TextBoxUpdateAll.Text, null);
                        break;

                }
            }
            else
            {
                dt.Rows.Add(id, drivertype, speed, Location, Link, null);
            }
       }

        DataView dvVehicle = new DataView(dt);

        ViewState["table"] = dt;
        BindingGridView();
   
    }
    protected void Pass_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.NoRequest);
    }
    protected void ButtonStartSim_Click(object sender, EventArgs e)
    {
        proxy.SetNumberOfConnectedClients(1);
        proxy.SetAPITimestepInterval(60);
        proxy.SetServerWriteTextFlag(0);
        proxy.SetServerTRFFile("C:\\etRunnerWeb\\Sample\\FreewayForWeb.TRF");
        proxy.SetClientState((int)ClientState.UseTRFFile);
        ShowNextTimeStep();
        ShowVehicles();
        PopulateEntryNodeList();
    }
    protected void ButtonStepSim_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.Continued);
        ShowNextTimeStep();
        ShowVehicles();
    }
    protected void TextBoxTimestep_TextChanged(object sender, EventArgs e)
    {

    }
    protected void ButtonEndSim_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.APIShutDown);
    }

    protected void ShowNextTimeStep()
    {
        int state = proxy.GetClientState();
        while (state != (int)ClientState.DataReady)
        {
            state = proxy.GetClientState();
        }
        float nextTimeStep = proxy.GetServerTimestep() + 1;
        TextBoxCurTimeStep.Text = nextTimeStep.ToString();
    }

    private void PopulateEntryNodeList()
    {
        int size = proxy.GetServerEntryNodeDataSize();
        WCF_ENTRYNODES_DATA[] entrynodes = new WCF_ENTRYNODES_DATA[size];
        entrynodes =    proxy.GetServerEntryNodeData();

        for (int i = 0; i < size; ++i)
        {
            DropDownListNodeID.Items.Add(new ListItem(entrynodes[i].Node_ID.ToString(), (i+1).ToString()));
        }
    }
    protected void ButtonGetEntryNodes_Click(object sender, EventArgs e)
    {
        PopulateEntryNodeList();
    }
}
