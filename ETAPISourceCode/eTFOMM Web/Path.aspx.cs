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
        AddVehicleSubmitted = 6,
        SignalsDataSubmitted = 7,
        FixTimeControlSignalsDataSubmitted = 8,
        Continued = 9
    } ;

    
    protected void Page_Load(object sender, EventArgs e)
    {
        address = new EndpointAddress("http://localhost:6000/service");
        binding = new WSHttpBinding();
        factory = new ChannelFactory<IService1>(binding, address);
        proxy = factory.CreateChannel();

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

    protected void End_Click(object sender, EventArgs e)
    {
        //proxy.setClientState((int)ClientState.NoRequest);
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
        //proxy.setClientState((int)ClientState.AddPathSubmitted); 
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
