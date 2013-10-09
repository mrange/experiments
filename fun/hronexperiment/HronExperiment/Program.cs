using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.Serialization;

using HronExperiment.Source.HRON;
using HronExperiment.Source.Common;

namespace HronExperiment
{
    class Program
    {
        static List<GenericRelationContract> GenerateTestData ()
        {
            var list = new List<GenericRelationContract>();

            const int Multiplier = 10;
            const int Count = 10;

            int cc = 0;
            for (int adr0 = 0; adr0 < Multiplier; ++adr0)
            {
                for (int adr1 = 0; adr1 < Multiplier; ++adr1)
                {
                    for (int adr2 = 0; adr2 < Multiplier; ++adr2)
                    {
                        for (int adr3 = 0; adr3 < Multiplier; ++adr3)
                        {
                            for (int ii = 0; ii < Count; ++ii)
                            {
                                GenericRelationContract contract = new GenericRelationContract();

                                contract.Origin = GuidIdentifier.New();
                                contract.SourceOriginId = GuidIdentifier.New();
                                contract.TargetOriginId = GuidIdentifier.New();
                                contract.TypeId = StringIdentifier.Parse("0:STR:aType");
                                contract.CreatorId = StringIdentifier.Parse("0:STR:marcus.rosell@site.com");
                                contract.ModifierId = StringIdentifier.Parse("0:STR:marcus.rosell@site.com");
                                contract.Pinned = 1;
                                contract.FromDate = DateTime.Now;
                                contract.ToDate = DateTime.Now;
                                contract.Iteration = cc;

                                contract.Attributes["adr0"] = adr0.ToString();
                                contract.Attributes["adr1"] = "test" + adr1.ToString();
                                contract.Attributes["adr2"] = "smurf" + adr2.ToString();
                                contract.Attributes["adr3"] = "mupp" + adr3.ToString();

                                list.Add(contract);

                                cc++;
                            }
                        }
                    }
                }
            }

            return list;
        }

        static T TimeIt<T> (string name, Func<T> action)
        {
            var sw = new Stopwatch ();

            Console.WriteLine ("Beginning to execute {0}", name ?? "<NULL>");

            sw.Start ();

            var result = action ();

            sw.Stop ();

            Console.WriteLine ("Completed execution of {0}, it took {1:#,0} ms", name ?? "<NULL>", sw.Elapsed.TotalMilliseconds);

            return result;
        }

        static void Main(string[] args)
        {
            var testData = GenerateTestData ();
            Console.WriteLine ("Generated {0:#,0} objects", testData.Count);

            var hronSerialized = TimeIt ("HRON Serializer", () => HRONSerializer.ObjectAsString(testData));
            Console.WriteLine ("HRON document is {0:#,0} characters long", hronSerialized.Length);

            TimeIt ("HRON Deserializer", () => 
                {
                    List<GenericRelationContract> deserialized;
                    HRONObjectParseError[] errors;

                    HRONSerializer.TryParseObject(
                        0,
                        hronSerialized.ReadLines (),
                        out deserialized,
                        out errors
                        );

                    return deserialized;
                });

            var serializer = new DataContractSerializer(typeof(List<GenericRelationContract>));

            var xmlSerialized = TimeIt ("XML Serializer", () => 
                {
                    using (var ms = new MemoryStream ())
                    {
                        serializer.WriteObject (ms, testData);
                        return ms.ToArray ();
                    }
                });
            Console.WriteLine ("XML document is {0:#,0} bytes long", xmlSerialized.Length);

            TimeIt ("XML Serializer", () => 
                {
                    using (var ms = new MemoryStream (xmlSerialized))
                    {
                        return (List<GenericRelationContract>)serializer.ReadObject(ms);
                    }
                });
        }
    }
}
