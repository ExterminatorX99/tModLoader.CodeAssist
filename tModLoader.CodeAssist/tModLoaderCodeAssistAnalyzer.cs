using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using tModLoader.CodeAssist.Terraria.ID;

namespace tModLoader.CodeAssist
{
	[DiagnosticAnalyzer(LanguageNames.CSharp)]
	public class tModLoaderCodeAssistAnalyzer : DiagnosticAnalyzer
	{
		public const string ChangeMagicNumberToIDDiagnosticId = "ChangeMagicNumberToID";

		// You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
		// See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
		private static readonly LocalizableString ChangeMagicNumberToIDTitle = new LocalizableResourceString(nameof(Resources.ChangeMagicNumberToIDTitle), Resources.ResourceManager, typeof(Resources));
		private static readonly LocalizableString ChangeMagicNumberToIDMessageFormat = new LocalizableResourceString(nameof(Resources.ChangeMagicNumberToIDMessageFormat), Resources.ResourceManager, typeof(Resources));
		private static readonly LocalizableString ChangeMagicNumberToIDDescription = new LocalizableResourceString(nameof(Resources.ChangeMagicNumberToIDDescription), Resources.ResourceManager, typeof(Resources));
		private const string Category = "Terraria.ID";

		private static DiagnosticDescriptor ChangeMagicNumberToIDRule = new(ChangeMagicNumberToIDDiagnosticId, ChangeMagicNumberToIDTitle, ChangeMagicNumberToIDMessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: ChangeMagicNumberToIDDescription);

		public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(ChangeMagicNumberToIDRule); } }

		private List<FieldToIDTypeBinding> FieldToIDTypeBindings;

		private List<MethodParameterToIDTypeBinding> MethodParameterToIDTypeBindings;

		public override void Initialize(AnalysisContext context)
		{
			// TODO: Consider registering other actions that act on syntax instead of or in addition to symbols
			// See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information

			IdDictionary soundIDtoIDDictionary = IdDictionary.Create(typeof(SoundID), typeof(int));

			FieldToIDTypeBindings = new List<FieldToIDTypeBinding>
			{
				new("Terraria.Item", "createTile", "TileID", TileID.Search),
				new("Terraria.Item", "type", "ItemID", ItemID.Search),
				new("Terraria.Item", "shoot", "ProjectileID", ProjectileID.Search),
				new("Terraria.Item", "useStyle", "ItemUseStyleID", IdDictionary.Create(typeof(ItemUseStyleID), typeof(int))),
				new("Terraria.Item", "rare", "ItemRarityID", IdDictionary.Create(typeof(ItemRarityID), typeof(int))),
				new("Terraria.NPC", "type", "NPCID", NPCID.Search),
				new("Terraria.Main", "netMode", "NetmodeID", IdDictionary.Create(typeof(NetmodeID), typeof(int))),
				new("Terraria.ModLoader.ModTile", "soundType", "SoundID", soundIDtoIDDictionary),
				new("Terraria.ModLoader.ModTile", "dustType", "DustID", DustID.Search),
				new("Terraria.ModLoader.ModWall", "dustType", "DustID", DustID.Search)
			};

			// Could check parameter name, or check parameter list and index.
			MethodParameterToIDTypeBindings = new List<MethodParameterToIDTypeBinding>
			{
				new("Terraria.Item.CloneDefaults", "Terraria.Item.CloneDefaults(int)", new[] { "Int32" }, 0, "ItemID", ItemID.Search),
				new("Terraria.Item.SetDefaults", "Terraria.Item.SetDefaults(int)", new[] { "Int32" }, 0, "ItemID", ItemID.Search),
				new("Terraria.ModLoader.ModRecipe.AddTile", "Terraria.ModLoader.ModRecipe.AddTile(int)", new[] { "Int32" }, 0, "TileID", TileID.Search),
				new("Terraria.ModLoader.ModRecipe.AddIngredient", "Terraria.ModLoader.ModRecipe.AddIngredient(int, int)", new[] { "Int32", "Int32" }, 0, "ItemID", ItemID.Search),
				new("Terraria.ModLoader.ModRecipe.SetResult", "Terraria.ModLoader.ModRecipe.SetResult(int, int)", new[] { "Int32", "Int32" }, 0, "ItemID", ItemID.Search),
				new("Terraria.NetMessage.SendData", "Terraria.NetMessage.SendData(int, int, int, Terraria.Localization.NetworkText, int, float, float, float, int, int, int)", new[] { "Int32", "Int32", "Int32", "NetworkText", "Int32", "Single", "Single", "Single", "Int32", "Int32", "Int32" }, 0, "MessageID", IdDictionary.Create(typeof(MessageID), typeof(byte))),
				new("Terraria.Main.PlaySound", "Terraria.Main.PlaySound(int, Microsoft.Xna.Framework.Vector2, int)", new[] { "Int32", "Vector2", "Int32" }, 0, "SoundID", soundIDtoIDDictionary),
				new("Terraria.Main.PlaySound", "Terraria.Main.PlaySound(int, int, int, int, float, float)", new[] { "Int32", "Int32", "Int32", "Int32", "Single", "Single" }, 0, "SoundID", soundIDtoIDDictionary),
				new("Terraria.Projectile.NewProjectile", "Terraria.Projectile.NewProjectile(Microsoft.Xna.Framework.Vector2, Microsoft.Xna.Framework.Vector2, int, int, float, int, float, float)", new[] { "Vector2", "Vector2", "Int32", "Int32", "Single", "Int32", "Single", "Single" }, 2, "ProjectileID", ProjectileID.Search),
				new("Terraria.Projectile.NewProjectile", "Terraria.Projectile.NewProjectile(float, float, float, float, int, int, float, int, float, float)", new[] { "Single", "Single", "Single", "Single", "Int32", "Int32", "Single", "Int32", "Single", "Single" }, 4, "ProjectileID", ProjectileID.Search),
				new("Terraria.Projectile.NewProjectileDirect", "Terraria.Projectile.NewProjectileDirect(Microsoft.Xna.Framework.Vector2, Microsoft.Xna.Framework.Vector2, int, int, float, int, float, float)", new[] { "Vector2", "Vector2", "Int32", "Int32", "Single", "Int32", "Single", "Single" }, 2, "ProjectileID", ProjectileID.Search),
				new("Terraria.Dust.NewDust", "Terraria.Dust.NewDust(Microsoft.Xna.Framework.Vector2, int, int, int, float, float, int, Microsoft.Xna.Framework.Color, float)", new[] { "Vector2", "Int32", "Int32", "Int32", "Single", "Single", "Int32", "Color", "Single" }, 3, "DustID", DustID.Search),
				new("Terraria.Dust.NewDustDirect", "Terraria.Dust.NewDustDirect(Microsoft.Xna.Framework.Vector2, int, int, int, float, float, int, Microsoft.Xna.Framework.Color, float)", new[] { "Vector2", "Int32", "Int32", "Int32", "Single", "Single", "Int32", "Color", "Single" }, 3, "DustID", DustID.Search)
			};
			//MethodParameterToIDTypeBindings.Add(new MethodParameterToIDTypeBinding("Terraria.Dust.NewDustPerfect", "Terraria.Dust.NewDustPerfect(Microsoft.Xna.Framework.Vector2, int, Microsoft.Xna.Framework.Vector2?, int, Microsoft.Xna.Framework.Color, float)", new string[] { "Vector2", "Int32", "Vector2?", "Int32", "Color", "Single" }, 1, "DustID", DustID.Search));

			// Main.rand.Next(x) == 0 => Main.rand.NextBool(x)

			//modTile.drop, modTile.soundType, modTile.dustType
			// using static ModContent
			// Detect bad AddTile AddIngredient
			// Main.player[Main.myPlayer] => Main.LocalPlayer
			// new Vector2(player.position.X + player.width / 2, player.position.Y + player.height / 2) => player.Center

			context.EnableConcurrentExecution();

			context.RegisterSyntaxNodeAction(AnalyzeMagicNumberAssignmentExpressions, SyntaxKind.SimpleAssignmentExpression);

			context.RegisterSyntaxNodeAction(AnalyzeMagicNumberEqualsExpressions, SyntaxKind.EqualsExpression,
																					SyntaxKind.NotEqualsExpression,
																					SyntaxKind.GreaterThanExpression,
																					SyntaxKind.GreaterThanOrEqualExpression,
																					SyntaxKind.LessThanExpression,
																					SyntaxKind.LessThanOrEqualExpression);

			context.RegisterSyntaxNodeAction(AnalyzeMagicNumberInvocationExpressions, SyntaxKind.InvocationExpression);

		   // context.RegisterSyntaxNodeAction(AnalyzeIncorrectParameterInvocationExpressions, SyntaxKind.InvocationExpression);
		}

		private void AnalyzeIncorrectParameterInvocationExpressions(SyntaxNodeAnalysisContext context)
		{
			// if Method name exists in list

			// if 

			// Detect bad AddTile AddIngredient -> Check for presence of ItemID or call to ItemType
		}

		private void AnalyzeMagicNumberInvocationExpressions(SyntaxNodeAnalysisContext context)
		{
			var invocationExpressionSyntax = (InvocationExpressionSyntax)context.Node;

			if (!(invocationExpressionSyntax.Expression is MemberAccessExpressionSyntax MemberAccessExpressionSyntax))
				return;
			 
			string methodName = MemberAccessExpressionSyntax.Name.ToString();
			//if (MemberAccessExpressionSyntax?.Name.ToString() != "AddIngredient")
			//    return;

			var argumentListSyntax = invocationExpressionSyntax.ArgumentList;
			if (argumentListSyntax == null)
				return;
			int argumentCount = argumentListSyntax.Arguments.Count;


			//if (argumentCount != 1 && argumentCount != 2) 
			//    return;

			var memberSymbol = context.SemanticModel.GetSymbolInfo(MemberAccessExpressionSyntax).Symbol as IMethodSymbol;
			if (memberSymbol == null)
				return;

			//if (argumentCount != memberSymbol.Parameters.Length) // < is fine, optional?
			//    return;

			string fullyQualifiedMethodName = memberSymbol.ToString();

			//   if (!memberSymbol.ToString().StartsWith("Terraria.ModLoader.ModRecipe.AddIngredient") ?? true) return;

			var parameterTypeNames = memberSymbol.Parameters.Select(p => p.Type.Name);

			// Find exact MethodParameterToIDTypeBinding related to this InvocationExpressionSyntax
			var methodParameterToIDTypeBinding = MethodParameterToIDTypeBindings.FirstOrDefault(x => x.FullMethodWithParameters == fullyQualifiedMethodName && x.ParameterNames.SequenceEqual(parameterTypeNames));
			if (methodParameterToIDTypeBinding == null)
				return;

			if (argumentCount < methodParameterToIDTypeBinding.ParameterIndex)
				return;

			// Check if parameter at index is literal number: SetDefaults(111, 2)
			if (!(argumentListSyntax.Arguments[methodParameterToIDTypeBinding.ParameterIndex].Expression is LiteralExpressionSyntax parameter && parameter.IsKind(SyntaxKind.NumericLiteralExpression)))
				return;
			//if (!(memberSymbol.Parameters[methodParameterToIDTypeBinding.parameterIndex] is LiteralExpressionSyntax right && right.IsKind(SyntaxKind.NumericLiteralExpression)))
			//     return;

			//  methodParameterToIDTypeBinding.parameterIndex


			//if (p.Length != 2)
			//    return;

			//if (p[0].Type.Name != "Int32" || p[1].Type.Name != "Int32")
			//    return;

			Console.WriteLine();

			int rightValue = (int)parameter.Token.Value;

			if (methodParameterToIDTypeBinding.IDDictionary.ContainsId(rightValue))
			//if (rightValue > 0 && rightValue < methodParameterToIDTypeBinding.idDictionary.Count /* ItemID.Count*/)
			{
				//string result = "ItemID." + ItemID.Search.GetName(rightValue);
				string result = $"{methodParameterToIDTypeBinding.IDType}.{methodParameterToIDTypeBinding.IDDictionary.GetName(rightValue)}"; // + ItemID.Search.GetName(rightValue);

				var builder = ImmutableDictionary.CreateBuilder<string, string>();
				builder["result"] = result;
				builder["idType"] = methodParameterToIDTypeBinding.IDType;
				var properties = builder.ToImmutable();

				var diagnostic = Diagnostic.Create(ChangeMagicNumberToIDRule, parameter.GetLocation(), properties, rightValue, result);
				context.ReportDiagnostic(diagnostic);
			}

		}

		// assignment and equals can share code.

		private void AnalyzeMagicNumberEqualsExpressions(SyntaxNodeAnalysisContext context)
		{
			// Only support EqualsExpression: a == b
			var binaryExpressionSyntax = (BinaryExpressionSyntax)context.Node;

			// Check if right side is literal number: a == 123
			if (!(binaryExpressionSyntax.Right is LiteralExpressionSyntax right && right.IsKind(SyntaxKind.NumericLiteralExpression)))
				return;

			ISymbol symbol;
			// Check if left is just a field: a = 123
			if (binaryExpressionSyntax.Left is IdentifierNameSyntax identifierNameSyntax)
			{
				symbol = context.SemanticModel.GetSymbolInfo(identifierNameSyntax).Symbol;
			}
			// Check if left is accessing a member: a.b = 123
			else if (binaryExpressionSyntax.Left is MemberAccessExpressionSyntax memberAccessExpressionSyntax)
			{
				symbol = context.SemanticModel.GetSymbolInfo(memberAccessExpressionSyntax).Symbol;
			}
			else
				return;

			// Check if left Type exists: item.b = 123
			if (symbol == null || symbol.ContainingType == null)
				return;

			if (!(symbol is IFieldSymbol fieldSymbol))
				return;

			string containingType = symbol.ContainingType.ToString();

			string fieldName = fieldSymbol.Name;
			var FieldToIDTypeBinding = FieldToIDTypeBindings.FirstOrDefault(x => x.FullyQualifiedClassName == containingType && x.Field == fieldName);
			if (FieldToIDTypeBinding == null)
				return;

			int rightValue = (int)right.Token.Value;

			if(FieldToIDTypeBinding.IDDictionary.ContainsId(rightValue))
			//if (rightValue > 0 && rightValue < FieldToIDTypeBinding.idDictionary.Count /* ItemID.Count*/)
			{
				string result = $"{FieldToIDTypeBinding.IDType}.{FieldToIDTypeBinding.IDDictionary.GetName(rightValue)}";

				var builder = ImmutableDictionary.CreateBuilder<string, string>();
				builder["result"] = result;
				builder["idType"] = FieldToIDTypeBinding.IDType;
				var properties = builder.ToImmutable();

				var diagnostic = Diagnostic.Create(ChangeMagicNumberToIDRule, right.GetLocation(), properties, rightValue, result);
				context.ReportDiagnostic(diagnostic);
			}
		}

		private void AnalyzeMagicNumberAssignmentExpressions(SyntaxNodeAnalysisContext context)
		{
			// Only support simple assignment: a = b
			var assignmentExpressionSyntax = (AssignmentExpressionSyntax)context.Node;
			if (!assignmentExpressionSyntax.IsKind(SyntaxKind.SimpleAssignmentExpression))
				return;

			// Check if right side is literal number: a = 123
			if (!(assignmentExpressionSyntax.Right is LiteralExpressionSyntax right && right.IsKind(SyntaxKind.NumericLiteralExpression)))
				return;

			ISymbol symbol;
			// Check if left is just a field: a = 123
			if (assignmentExpressionSyntax.Left is IdentifierNameSyntax identifierNameSyntax)
			{
				symbol = context.SemanticModel.GetSymbolInfo(identifierNameSyntax).Symbol;
			}
			// Check if left is accessing a member: a.b = 123
			else if (assignmentExpressionSyntax.Left is MemberAccessExpressionSyntax memberAccessExpressionSyntax)
			{
				symbol = context.SemanticModel.GetSymbolInfo(memberAccessExpressionSyntax).Symbol;
			}
			else
				return;

			// Check if left Type exists: item.b = 123
			if (symbol == null || symbol.ContainingType == null)
				return;

			if (!(symbol is IFieldSymbol fieldSymbol))
				return;

			string containingType = symbol.ContainingType.ToString();
			//if (!containingType.Equals("Terraria.Item"))
			//     return;

			string fieldName = fieldSymbol.Name;
			var FieldToIDTypeBinding = FieldToIDTypeBindings.FirstOrDefault(x => x.FullyQualifiedClassName == containingType && x.Field == fieldName);
			if (FieldToIDTypeBinding == null)
				return;

			int rightValue = (int)right.Token.Value;

			if (FieldToIDTypeBinding.IDDictionary.ContainsId(rightValue))
			//if (rightValue > 0 && rightValue < FieldToIDTypeBinding.idDictionary.Count /* ItemID.Count*/)
			{
				//string result = "ItemID." + ItemID.Search.GetName(rightValue);
				string result = $"{FieldToIDTypeBinding.IDType}.{FieldToIDTypeBinding.IDDictionary.GetName(rightValue)}"; // + ItemID.Search.GetName(rightValue);

				var builder = ImmutableDictionary.CreateBuilder<string, string>();
				builder["result"] = result;
				builder["idType"] = FieldToIDTypeBinding.IDType;
				var properties = builder.ToImmutable();

				var diagnostic = Diagnostic.Create(ChangeMagicNumberToIDRule, right.GetLocation(), properties, rightValue, result);
				context.ReportDiagnostic(diagnostic);
			}
		}
	}
}
