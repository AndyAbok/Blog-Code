// using System.Reflection;
// using FSharpInterop;

// var method =
//     typeof(FSharpUtil).GetMethod("Method");

// var param = method.GetParameters()[0];

// Console.WriteLine($"Type: {param.ParameterType}");
// Console.WriteLine($"IsOptional: {param.IsOptional}");
// Console.WriteLine($"HasDefaultValue: {param.HasDefaultValue}");


var util = new CSharpUtil();

util.Method();

public class CSharpUtil
{
    public void Method(int i = 42)
    {
        Console.WriteLine(i);
    }
}


