import java.awt.*;
import java.awt.event.*;
import amzi.arulesxl.*;
import amzi.ls.LogicServer;
import amzi.ls.LSException;

public class Advice extends Frame
                  implements ActionListener
{
	 // Current directory passed to openRules()
   private static final String SAMPLE_DIR = "C:/amzi/source/arules/api/java/samples/advice/"; 

   private TextField swingSpeed;
   private Choice clubType;
   private Choice favor;
   private Choice ballFlight;
   private TextArea advice;
   private ARulesXL arxl;

   public Advice()
   {
      Panel buttons = new Panel();
      buttons.setLayout(new FlowLayout());
      Button go = new Button("Go");
      go.addActionListener(this);
      buttons.add(go);
      Button exit = new Button("Exit");
      exit.addActionListener(this);
      buttons.add(exit);
      add("South", buttons);

      Panel inputs = new Panel();
      inputs.setLayout(new FlowLayout(FlowLayout.LEFT));

      inputs.add(new Label("Swing Speed"));
      swingSpeed = new TextField("0", 5);
      inputs.add(swingSpeed);

			inputs.add(new Label("Club Type"));
      clubType = new Choice();
      clubType.add("Driver < 11 Degrees");
      clubType.add("Driver >= 11 Degrees");
      clubType.add("Fairway Wood");
      clubType.add("Hybrid/Utility");
      clubType.add("Iron");
      clubType.add("Wedge");
      inputs.add(clubType);

			inputs.add(new Label("Favor"));
      favor = new Choice();
      favor.add("Distance");
      favor.add("Accuracy");
      inputs.add(favor);

			inputs.add(new Label("Ball Flight"));
      ballFlight = new Choice();
      ballFlight.add("Normal");
      ballFlight.add("High");
      ballFlight.add("Low");
      inputs.add(ballFlight);
      
      add("North", inputs);

      advice = new TextArea(1, 40);
      add("Center", advice);

      addWindowListener(new DWAdapter());
   }

   class DWAdapter extends WindowAdapter 
   {
      public void windowClosing(WindowEvent event) 
      {
         System.exit(0);
      }
   }

   public void actionPerformed(ActionEvent event)
   {
      Button source = (Button)event.getSource();
      if (source.getLabel().equals("Go"))
      {
         callExpert();
         repaint();
      }
      if (source.getLabel().equals("Exit"))
         System.exit(0);
   }

   public void callExpert()
   {
			// Query for the history analysis and plan
			arxl = new ARulesXL();
			try {
				// Open the ruleset
				arxl.openRules("advice.axl", SAMPLE_DIR);
	
				// Clear then set the input data, .in[*]
				arxl.clearVector("ShaftRules", "in");
				arxl.addToVector("ShaftRules", "in", "Swing Speed", swingSpeed.getText());
				arxl.addToVector("ShaftRules", "in", "Club Type", clubType.getSelectedItem());
				arxl.addToVector("ShaftRules", "in", "Favor", favor.getSelectedItem());
				arxl.addToVector("ShaftRules", "in", "Ball Flight", ballFlight.getSelectedItem());

				// Query for the advice
				String ans = arxl.queryRules("ShaftRules", "FIND advice");
				advice.setText(ans);
				arxl.closeRules();
			} 
			catch (Exception ex) {
				try {
					arxl.closeRules();
				} 
				catch (Exception e) {
				}
				System.out.println("Advice failed " + ex.getMessage());
			}
   }

   public static void displayStatus(String Msg)
   {
      System.out.println(Msg);
   }

   public static void main(String[] args)
   {
      Frame f = new Advice();
      f.setTitle("Shaft Advisor");
      f.show();     // Java 1.6 change to setVisible(true);
      f.pack();
   }

}

