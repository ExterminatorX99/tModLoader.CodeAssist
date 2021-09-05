using tModLoader.CodeAssist.Terraria.ID;

namespace tModLoader.CodeAssist
{
	public class MethodParameterToIDTypeBinding
	{
		public string FullyQualifiedMethodName;
		public string MethodName;
		public string FullMethodWithParameters;
		public string[] ParameterNames;
		public int ParameterIndex;
		public string IDType;
		public IdDictionary IDDictionary;

		public MethodParameterToIDTypeBinding(string fullyQualifiedMethodName, string fullMethodWithParameters, string[] parameterNames, int parameterIndex, string idType, IdDictionary idDictionary)
		{
			FullyQualifiedMethodName = fullyQualifiedMethodName;
			MethodName = fullyQualifiedMethodName.Substring(fullyQualifiedMethodName.LastIndexOf(".") + 1);
			FullMethodWithParameters = fullMethodWithParameters;
			ParameterNames = parameterNames;
			ParameterIndex = parameterIndex;
			IDType = idType;
			IDDictionary = idDictionary;
		}

		public override string ToString()
		{
			return $"{FullMethodWithParameters} {ParameterIndex} {IDType}";
		}
	}
}
