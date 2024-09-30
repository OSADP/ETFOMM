using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Data;
using System.ServiceModel;
using WCFServer;
using System.Collections;

public partial class _Default : System.Web.UI.Page
{
    //Connect WCF Service
    static int TimestepInterval;
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

    private class local_signal
    {
        public int nid;
        public int LEW;
        public int SEW;
        public int LNS;
        public int SNS;


        public local_signal(int nid)
        {
            this.nid = nid;
            this.LEW = 0;
            this.SEW = 0;
            this.LNS = 0;
            this.SNS = 0;
        }
    }
    
    protected void Page_Load(object sender, EventArgs e)
    {
        TimestepInterval = 60;
        address = new EndpointAddress("http://localhost:8000/service");
        binding = new WSHttpBinding();
        binding.MaxBufferPoolSize = 2147483647;
        binding.MaxReceivedMessageSize = 2147483647;
        binding.Security.Mode = SecurityMode.None;
        factory = new ChannelFactory<IService1>(binding, address);
        proxy = factory.CreateChannel();

        //TextBoxTimestep.Text = TimestepInterval.ToString();
    }


    protected void GetSignal_Click(object sender, EventArgs e)
    {
        ShowSignals();
    }


    private void ShowSignals()
    {
        int size = proxy.GetServerFTCSignalDataSize();
        gvSignal.DataSource = null;
        gvSignal.DataBind();

        WCF_FTC_DATA[] ftc = new WCF_FTC_DATA[size];
        ftc = proxy.GetServerFTCSignalData();

        DataTable table = new DataTable();
        table.Columns.Add("NodeID");
        table.Columns.Add("Straight_NS");
        table.Columns.Add("Left_NS");
        table.Columns.Add("Straight_EW");
        table.Columns.Add("Left_EW");
        table.Columns.Add("TID");

        table.Columns["TID"].AutoIncrement = true;
        table.Columns["TID"].AutoIncrementSeed = 1;
        table.Columns["TID"].AutoIncrementStep = 1;

        DataColumn[] dcKeys = new DataColumn[1];
        dcKeys[0] = table.Columns["NodeID"];
        table.PrimaryKey = dcKeys;

        ///////

        WCF_FTC_DATA Node5FTC = ftc[0];

        
        float id5 = Node5FTC.node;
        float SNS = Node5FTC.duration[0];
        float LNS = Node5FTC.duration[3];
        float SEW = Node5FTC.duration[6];
        float LEW = Node5FTC.duration[9];

        table.Rows.Add(id5, SNS, LNS, SEW, LEW, null);
        ///////
        DataView dvVehicle = new DataView(table);
        ViewState["table"] = table;
        BindingGridView();

        ShowCycleLength(ftc[0].cycle_length);
    }



    private void BindingGridView()
    {
        if (ViewState["table"] != null)
        {

            DataTable table = (DataTable)ViewState["table"];

            DataView dv = new DataView(table);

            // Bind the GridView control.
            gvSignal.DataSource = dv;
            gvSignal.DataBind();
        }
    }

    protected void gvSignal_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        gvSignal.PageIndex = e.NewPageIndex;

        BindingGridView();
    }

    protected void gvSignal_RowEditing(object sender, GridViewEditEventArgs e)
    {

        gvSignal.EditIndex = e.NewEditIndex;

        BindingGridView();
    }

    protected void gvSignal_RowCancelingEdit(object sender, GridViewCancelEditEventArgs e)
    {
        gvSignal.EditIndex = -1;

        BindingGridView();
    }

    // GridView.RowUpdating Event
    protected void gvSignal_RowUpdating(object sender, GridViewUpdateEventArgs e)
    {
        if (ViewState["table"] != null)
        {
            // Get the DataTable from ViewState.
            DataTable dt = (DataTable)ViewState["table"];

            // Get the ID of the selected row.
            string strID = gvSignal.Rows[e.RowIndex].Cells[2].Text;

            // Find the row in DateTable.
            DataRow dr = dt.Rows.Find(strID);

            // Retrieve edited values and updating respective items.

           // dr["NodeID"] = ((TextBox)gvSignal.Rows[e.RowIndex].FindControl("TextBox1")).Text;
            dr["Straight_NS"] = ((TextBox)gvSignal.Rows[e.RowIndex].FindControl("TextBox5")).Text;
            dr["Left_NS"] = ((TextBox)gvSignal.Rows[e.RowIndex].FindControl("TextBox4")).Text;
            dr["Straight_EW"] = ((TextBox)gvSignal.Rows[e.RowIndex].FindControl("TextBox3")).Text;
            dr["Left_EW"] = ((TextBox)gvSignal.Rows[e.RowIndex].FindControl("TextBox2")).Text;
            // Exit edit mode.
            gvSignal.EditIndex = -1;

            // Rebind the GridView control to show data after updating.
            BindingGridView();

        }
    }

    protected void gvSignal_Sorting(object sender, GridViewSortEventArgs e)
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

    protected void Submit_Click(object sender, EventArgs e)
    {
        DataTable table = (DataTable)ViewState["table"];
        WCF_FTC_DATA[] Wcf_FTC = proxy.GetServerFTCSignalData();
        
        //String id = ((Label)gvSignal.Rows[0].FindControl("Label1")).Text;
        String LEW = ((Label)gvSignal.Rows[0].FindControl("Label2")).Text;
        String SEW = ((Label)gvSignal.Rows[0].FindControl("Label3")).Text;
        String LNS = ((Label)gvSignal.Rows[0].FindControl("Label4")).Text;
        String SNS = ((Label)gvSignal.Rows[0].FindControl("Label5")).Text;

        //Wcf_FTC[1].id = Convert.ToInt32(id);
        Wcf_FTC[0].duration[0] = Convert.ToInt32(SNS );
        Wcf_FTC[0].duration[3] = Convert.ToInt32(LNS );
        Wcf_FTC[0].duration[6] = Convert.ToInt32(SEW);
        Wcf_FTC[0].duration[9] = Convert.ToInt32(LEW);
        Wcf_FTC[0].cycle_length = 0;
        for (int i = 0; i < Wcf_FTC[0].active_intervals; ++i)
        {
            Wcf_FTC[0].cycle_length += Wcf_FTC[0].duration[i];
        }

        proxy.SetServerFTCSignalData(Wcf_FTC);
        proxy.SetClientState((int)ClientState.FixTimeControlSignalsDataSubmitted);
        SubmitLabel.InnerText = "Submit Successfully!";

        ShowNextTimeStep();
        ShowSignals();
    }


    protected void TimeButton_Click(object sender, EventArgs e)
    {
        String timeinterval = TextBoxTimestep.Text;
        TimestepInterval = Convert.ToInt32(timeinterval);
        proxy.SetAPITimestepInterval(TimestepInterval);
    }

    protected void Pass_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.NoRequest);
    }

    protected void ButtonStartSim_Click(object sender, EventArgs e)
    {
        proxy.SetNumberOfConnectedClients(1);
        proxy.SetAPITimestepInterval(TimestepInterval);
        proxy.SetServerWriteTextFlag(1);
        proxy.SetServerTRFFile("C:\\etRunnerWeb\\Sample\\FTCSignalForWeb.TRF");
        proxy.SetClientState((int)ClientState.UseTRFFile);
        ShowNextTimeStep();
        TextBoxTimestep.Text = TimestepInterval.ToString();
        ShowSignals();
    }

    protected void ButtonStepSim_Click(object sender, EventArgs e)
    {
        proxy.SetClientState((int)ClientState.Continued);
        ShowNextTimeStep();
        ShowSignals();
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
        //int curTimePeriod = proxy.GetServerTimePeriod();
        //TextBoxCurTimePeriod.Text = curTimePeriod.ToString();
        float nextTimeStep = proxy.GetServerTimestep() + 1;
        TextBoxCurTimeStep.Text = nextTimeStep.ToString();
    }
    protected void gvSignal_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
    private void ShowCycleLength(float CycleLength)
    {
        LabelCycleLength.InnerText = "Cycle Length:" + CycleLength.ToString();
    }
}
