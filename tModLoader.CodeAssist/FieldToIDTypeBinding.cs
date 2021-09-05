using tModLoader.CodeAssist.Terraria.ID;

namespace tModLoader.CodeAssist
{
	public class FieldToIDTypeBinding
	{
		// Example: Item - createTile - TileID
		//Type className;
		public string FullyQualifiedClassName;
		public string ClassName;
		public string Field;
		//Type idType;
		public string IDType;
		public IdDictionary IDDictionary;

		public FieldToIDTypeBinding(string fullName, string field, string idType, IdDictionary idDictionary)
		{
			FullyQualifiedClassName = fullName;
			ClassName = fullName.Substring(fullName.LastIndexOf(".") + 1);
			Field = field;
			IDType = idType;
			IDDictionary = idDictionary;
		}

		public override string ToString()
		{
			return $"{FullyQualifiedClassName} {Field} {IDType}";
		}
	}
}
