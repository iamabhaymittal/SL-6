import java.io.IOException;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class MaxTemperature {
	public static class MaxTemperatureMapper extends Mapper<LongWritable, Text, Text, IntWritable> {
		private static final int MISSING = 9999;
		@Override
		public void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
				String line = value.toString();
				String year = line.substring(15, 19);
				int airTemperature;
				if (line.charAt(87) == '+') { // parseInt doesn't like leading plus signs
					airTemperature = Integer.parseInt(line.substring(88, 92));
				} 
				else {
					airTemperature = Integer.parseInt(line.substring(87, 92));
				}
				String quality = line.substring(92, 93);
				if (airTemperature != MISSING && quality.matches("[01459]")) {
					context.write(new Text(year), new IntWritable(airTemperature));
				}
		}
	}

	public static class MaxTemperatureReducer extends Reducer<Text, IntWritable, Text, IntWritable> {
		static int max = 0, min = 0;
		Text minYear=new Text();
		Text maxYear=new Text();
		@Override
		public void reduce(Text key, Iterable<IntWritable> values, Context context) throws IOException, InterruptedException {
			//int maxValue = Integer.MIN_VALUE;
			int avgValue=0,count=0;
			String year = key.toString();
			for (IntWritable value : values) {
				avgValue+=value.get();
				count=count+1;
			}
			avgValue/=count;
			
			if(year.equals("1901")==true) {
				max = avgValue;
				min = avgValue;
			}
			try {
				if(avgValue > max) {
					max = avgValue;
					maxYear.set(key);
				}
		
				if(avgValue < min) {
					min = avgValue;
					minYear.set(key);
				}
			}
			catch(NullPointerException e) {
			}
			if(year.equals("1920")==true) {
				context.write(maxYear, new IntWritable(max));
				context.write(minYear, new IntWritable(min));
			}	
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 2) {
			System.err.println("Usage: MaxTemperature <input path> <output path>");
			System.exit(-1);
		}
		Job job = new Job();
		job.setJarByClass(MaxTemperature.class);
		job.setJobName("Max temperature");
		FileInputFormat.addInputPath(job, new Path(args[0]));
		FileOutputFormat.setOutputPath(job, new Path(args[1]));
		job.setMapperClass(MaxTemperatureMapper.class);
		job.setReducerClass(MaxTemperatureReducer.class);
		job.setOutputKeyClass(Text.class);
		job.setOutputValueClass(IntWritable.class);
		System.exit(job.waitForCompletion(true) ? 0 : 1);
	}
}