using System;
using System.Collections.Generic;
using System.Runtime.Serialization;

namespace HronExperiment
{
    public enum PersistentState
    {
        NonPersistent = 0,
        Local = 1,
        Persistent = 2,
        Global = 3,
    }

    public enum IdentifierType
    {
        String = 0,
        Guid = 1,
    }

    public interface IIdentifier : IComparable
    {
        PersistentState State { get; set; }
        string Value { get; }
        string Compound { get; set; }
    }

    [DataContract]
    public class Byte12Identifier : IIdentifier
    {
        static object mLocalIdLock = new object();
        static int mLocalId = 1;

        PersistentState mType = PersistentState.NonPersistent;
        public PersistentState State
        {
            get { return mType; }
            set { mType = value; }
        }

        string mValue = null;
        public string Value
        {
            get { return mValue; }
            set { mValue = value; }
        }

        public Byte12Identifier ()
        {
        }

        public Byte12Identifier(PersistentState state, string value)
        {
            mType = (PersistentState)state;
            mValue = value;
        }

        public override string ToString()
        {
            return this.Compound;
        }

        [DataMember]
        public string Compound
        {
            get
            {
                return String.Format("{0}:{1}:{2}", (int)mType, "B12", mValue);
            }
            set
            {
                Byte12Identifier that = Parse(value);
                this.mType = that.mType;
                this.mValue = that.mValue;
            }
        }

        public int CompareTo(object obj)
        {
            Byte12Identifier b12 = (Byte12Identifier)obj;
            if (b12.mType == mType)
            {
                return this.mValue.CompareTo(b12.mValue);
            }
            else
                return this.mType.CompareTo(b12.mType);
        }

        // statics
        static public Byte12Identifier Parse(string strId)
        {
            string[] arr = strId.Split(new char[] { ':' });
            if (arr.Length == 3)
            {
                int t;
                t = Int32.Parse(arr[0]);
                if (arr[1] != "B12")
                    throw new ArgumentException("id, not of B12 type");

                Byte12Identifier id = new Byte12Identifier((PersistentState)t, arr[2]);
                return id;
            }
            else
                throw new ArgumentException("id, of unknown format");
        }

        static public Byte12Identifier New()
        {
            int id;
            lock (mLocalIdLock)
            {
                id = mLocalId++;
                return new Byte12Identifier(PersistentState.NonPersistent, id.ToString());
            }
        }

        static public string ToString(PersistentState type, string value)
        {
            return ((int)type).ToString() + ":B12:" + value;
        }
    }

    [DataContract]
    public class GuidIdentifier : IIdentifier
    {
        PersistentState mType = PersistentState.NonPersistent;
        public PersistentState State
        {
            get { return mType; }
            set { mType = value; }
        }

        string mValue;
        public string Value
        {
            get { return mValue; }
            set { mValue = value; }
        }

        public Guid Native
        {
            get { return new Guid(mValue); }
        }

        public GuidIdentifier ()
        {
        }

        public GuidIdentifier(string value)
        {
            mType = PersistentState.Global;
            mValue = value;
        }

        public override string ToString()
        {
            return this.Compound;
        }

        [DataMember]
        public string Compound
        {
            get
            {
                return String.Format("{0}:{1}:{2}", (int)mType, "GUID", mValue.ToString());
            }
            set
            {
                GuidIdentifier that = Parse(value);
                this.mType = that.mType;
                this.mValue = that.mValue;
            }
        }

        public int CompareTo(object obj)
        {
            GuidIdentifier guid = (GuidIdentifier)obj;
            return this.mValue.CompareTo(guid.mValue);
        }

        // statics
        static public GuidIdentifier Parse(string strId)
        {
            string[] arr = strId.Split(new char[] { ':' });
            if (arr.Length == 3)
            {
                //                int t;
                //                t = Int32.Parse(arr[0]);
                if (arr[1] != "GUID")
                    throw new ArgumentException("id, not of GUID type");

                GuidIdentifier id = new GuidIdentifier(arr[2]);
                return id;
            }
            else
                throw new ArgumentException("id, of unknown format");
        }

        static public GuidIdentifier New()
        {
            return new GuidIdentifier(Guid.NewGuid().ToString());
        }

        static public string ToString(PersistentState type, string value)
        {
            return ((int)type).ToString() + ":GUID:" + value;
        }
    }

    [DataContract]
    public class StringIdentifier : IIdentifier
    {
        static object mLocalIdLock = new object();
        static int mLocalId = 1;

        PersistentState mType = PersistentState.NonPersistent;
        public PersistentState State
        {
            get { return mType; }
            set { mType = value; }
        }

        string mValue = null;
        public string Value
        {
            get { return mValue; }
            set { mValue = value; }
        }

        public StringIdentifier()
        {
        }

        public StringIdentifier(PersistentState state, string value)
        {
            mType = (PersistentState)state;
            mValue = value;
        }

        public override string ToString()
        {
            return this.Compound;
        }

        [DataMember]
        public string Compound
        {
            get
            {
                return String.Format("{0}:{1}:{2}", (int)mType, "STR", mValue);
            }
            set
            {
                StringIdentifier that = Parse(value);
                this.mType = that.mType;
                this.mValue = that.mValue;
            }
        }

        public int CompareTo(object obj)
        {
            StringIdentifier strId = (StringIdentifier)obj;
            if (strId.mType == mType)
            {
                return this.mValue.CompareTo(strId.mValue);
            }
            else
                return this.mType.CompareTo(strId.mType);
        }

        // statics
        static public StringIdentifier Parse(string strId)
        {
            string[] arr = strId.Split(new char[] { ':' });
            if (arr.Length == 3)
            {
                int t;
                t = Int32.Parse(arr[0]);
                if (arr[1] != "STR")
                    throw new ArgumentException("id, not of STR type");

                StringIdentifier id = new StringIdentifier((PersistentState)t, arr[2]);
                return id;
            }
            else
                throw new ArgumentException("id, of unknown format");
        }

        static public StringIdentifier New()
        {
            int id;
            lock (mLocalIdLock)
            {
                id = mLocalId++;
                return new StringIdentifier(PersistentState.NonPersistent, id.ToString());
            }
        }

        static public string ToString(PersistentState type, string value)
        {
            return ((int)type).ToString() + ":STR:" + value;
        }
    }

    [DataContract]
    public class GenericRelationContract
    {
        [DataMember]
        public Byte12Identifier ObjectId;    // relation_id
        [DataMember]
        public GuidIdentifier SourceOriginId;
        [DataMember]
        public GuidIdentifier TargetOriginId;

        [DataMember]
        public StringIdentifier TypeId;
        [DataMember]
        public StringIdentifier CreatorId;
        [DataMember]
        public StringIdentifier ModifierId;

        [DataMember]
        public GuidIdentifier Origin;
        [DataMember]
        public int Pinned;
        [DataMember]
        public DateTime FromDate;
        [DataMember]
        public DateTime ToDate;
        [DataMember]
        public int Iteration;

        [DataMember]
        public Dictionary<string, object> Attributes = new Dictionary<string, object>();
    }

}
