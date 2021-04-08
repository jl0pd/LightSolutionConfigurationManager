﻿// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.
//-----------------------------------------------------------------------
// </copyright>

using System;
using SolutionParser.Shared;

namespace SolutionParser.Utilities
{
    /// <summary>
    ///     Represents toggleable features of the MSBuild engine
    /// </summary>
    internal class Traits
    {
        public static Traits Instance = new Traits();

        public EscapeHatches EscapeHatches { get; }

        /// <summary>
        /// Do not expand wildcards that match a certain pattern
        /// </summary>
        public readonly bool UseLazyWildCardEvaluation = !string.IsNullOrEmpty(Environment.GetEnvironmentVariable("MsBuildSkipEagerWildCardEvaluationRegexes"));
        public readonly bool LogExpandedWildcards = !string.IsNullOrEmpty(Environment.GetEnvironmentVariable("MSBUILDLOGEXPANDEDWILDCARDS"));
        public readonly bool CacheFileExistence = !string.IsNullOrEmpty(Environment.GetEnvironmentVariable("MsBuildCacheFileExistence"));

        /// <summary>
        /// Eliminate locking in OpportunisticIntern at the expense of memory
        /// </summary>
        public readonly bool UseSimpleInternConcurrency = !string.IsNullOrEmpty(Environment.GetEnvironmentVariable("MsBuildUseSimpleInternConcurrency"));

        /// <summary>
        /// Cache wildcard expansions
        /// </summary>
        public readonly bool MSBuildCacheFileEnumerations = !string.IsNullOrEmpty(Environment.GetEnvironmentVariable("MsBuildCacheFileEnumerations"));

        public Traits()
        {
            EscapeHatches = new EscapeHatches();
        }

        public readonly bool EnableAllPropertyFunctions = Environment.GetEnvironmentVariable("MSBUILDENABLEALLPROPERTYFUNCTIONS") == "1";
    }

    internal class EscapeHatches
    {
        /// <summary>
        /// Always use the accurate-but-slow CreateFile approach to timestamp extraction.
        /// </summary>
        public readonly bool AlwaysUseContentTimestamp = Environment.GetEnvironmentVariable("MSBUILDALWAYSCHECKCONTENTTIMESTAMP") == "1";

        public readonly bool LogProjectImports = Environment.GetEnvironmentVariable("MSBUILDLOGIMPORTS") == "1";

        /// <summary>
        /// Read information only once per file per ResolveAssemblyReference invocation.
        /// </summary>
        public readonly bool CacheAssemblyInformation = Environment.GetEnvironmentVariable("MSBUILDDONOTCACHERARASSEMBLYINFORMATION") != "1";

        public readonly ProjectInstanceTranslationMode? ProjectInstanceTranslation = ComputeProjectInstanceTranslation();

        /// <summary>
        /// Never use the slow (but more accurate) CreateFile approach to timestamp extraction.
        /// </summary>
        public readonly bool UseSymlinkTimeInsteadOfTargetTime = Environment.GetEnvironmentVariable("MSBUILDUSESYMLINKTIMESTAMP") == "1";

        /// <summary>
        /// Whether or not to ignore imports that are considered empty.  See ProjectRootElement.IsEmptyXmlFile() for more info.
        /// </summary>
        public readonly bool IgnoreEmptyImports = Environment.GetEnvironmentVariable("MSBUILDIGNOREEMPTYIMPORTS") == "1";

        /// <summary>
        /// Whether to to respect the TreatAsLocalProperty parameter on the Project tag.
        /// </summary>
        public readonly bool IgnoreTreatAsLocalProperty = Environment.GetEnvironmentVariable("MSBUILDIGNORETREATASLOCALPROPERTY") != null;

        /// <summary>
        /// Whether to write information about why we evaluate to debug output.
        /// </summary>
        public readonly bool DebugEvaluation = Environment.GetEnvironmentVariable("MSBUILDDEBUGEVALUATION") != null;

        /// <summary>
        /// Whether to warn when we set a property for the first time, after it was previously used.
        /// </summary>
        public readonly bool WarnOnUninitializedProperty = !String.IsNullOrEmpty(Environment.GetEnvironmentVariable("MSBUILDWARNONUNINITIALIZEDPROPERTY"));

        // MSBUILDUSECASESENSITIVEITEMNAMES is an escape hatch for the fix
        // for https://github.com/Microsoft/msbuild/issues/1751. It should
        // be removed (permanently set to false) after establishing that
        // it's unneeded (at least by the 16.0 timeframe).
        public readonly bool UseCaseSensitiveItemNames = Environment.GetEnvironmentVariable("MSBUILDUSECASESENSITIVEITEMNAMES") == "1";

        private static ProjectInstanceTranslationMode? ComputeProjectInstanceTranslation()
        {
            var mode = Environment.GetEnvironmentVariable("MSBUILD_PROJECTINSTANCE_TRANSLATION_MODE");

            if (mode == null)
            {
                return null;
            }

            if (mode.Equals("full", StringComparison.OrdinalIgnoreCase))
            {
                return ProjectInstanceTranslationMode.Full;
            }

            if (mode.Equals("partial", StringComparison.OrdinalIgnoreCase))
            {
                return ProjectInstanceTranslationMode.Partial;
            }

            ErrorUtilities.ThrowInternalError($"Invalid escape hatch for project instance translation: {mode}");

            return null;
        }

        public enum ProjectInstanceTranslationMode
        {
            Full,
            Partial
        }
    }
}
