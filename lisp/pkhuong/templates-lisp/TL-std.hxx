#define TL_EVAL(expr) TL:: expr ::eval::result

namespace TL_core
{
	struct dummyType {};
	struct Box {};

	template <int Int>
	struct BoxedInt : public Box
	{
		static const int value = Int;
		typedef BoxedInt<Int> result;
	};

	template <bool Bool>
	struct BoxedBool : public Box
	{
		static const bool value = Bool;
		typedef BoxedBool<Bool> result;
	};

	typedef BoxedBool<false> False;
	typedef BoxedBool<true>  True;

	//Do *NOT* call from TL code, since it
	//breaks the invariant that only types
	//are manipulated.
	template <typename T>
	struct Unbox
	{
		typedef T value;
	};

	template <int Int>
	struct Unbox< BoxedInt<Int> >
	{
		static const int value = Int;
	};

	template <bool Bool>
	struct Unbox <BoxedBool<Bool> >
	{
		static const bool value = Bool;
	};

	struct Nil
	{
		typedef Nil car;
		typedef Nil cdr;
		typedef Nil result;
	};

	template <typename theCar, typename theCdr>
	struct ConsCell
	{
		typedef theCar car;
		typedef theCdr cdr;
	};
	
	struct Cons
	{
		template <typename theCar, typename theCdr, typename dummy=dummyType>
		struct call
		{
			typedef ConsCell<theCar, theCdr> result;
		};
	};
	
	struct Car
	{
		template <typename T, typename dummy>
		struct call
		{
		};

		template <typename theCar, typename theCdr, typename dummy>
		struct call< ConsCell<theCar, theCdr>, dummy >
		{
			typedef theCar result;
		};
		template <typename dummy>
		struct call < Nil, dummy>
		{
			typedef Nil result;
		};
	};
	
	struct Cdr
	{
		template <typename T, typename dummy>
		struct call
		{
		};
		
		template <typename dummy, typename theCar, typename theCdr>
		struct call< ConsCell<theCar, theCdr>, dummy >
		{
			typedef theCdr result;
		};

		template <typename dummy>
		struct call <Nil, dummy>
		{
			typedef Nil result;
		};
	};

	struct Equal
	{
		template <typename A, typename B, typename dummy>
		struct call
		{
			typedef False result;
		};
		
		template <typename A, typename dummy>
		struct call<A, A, dummy>
		{
			typedef True result;
		};

	};

	struct Neg
	{
		template <typename A, typename dummy>
		struct call;
		
		template <int A, typename dummy>
		struct call < BoxedInt<A>, dummy >
		{
			typedef BoxedInt<-A> result;
		};
	};

	struct Add
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A + B> result;
		};
	};

	struct Sub
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A - B> result;
		};
	};

	struct Mul
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A * B> result;
		};
	};

	struct Div
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A / B> result;
		};
	};

	struct Mod
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A % B> result;
		};
	};

	struct Logxor
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A ^ B> result;
		};

		template <bool A, bool B, typename dummy>
		struct call <BoxedBool<A>, BoxedBool<B>, dummy>
		{
			typedef BoxedBool<A ^ B> result;
		};
	};

	struct Logand
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A & B> result;
		};
	};

	struct Logior
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A | B> result;
		};
	};

	struct Lognot
	{
		template <typename A, typename dummy>
		struct call;
		
		template <int A, typename dummy>
		struct call < BoxedInt<A>, dummy >
		{
			typedef BoxedInt<~A> result;
		};
	};

	struct Not
	{
		template <typename A, typename dummy>
		struct call;
		
		template <bool A, typename dummy>
		struct call < BoxedBool<A>, dummy >
		{
			typedef BoxedBool<!A> result;
		};
	};

	struct Gt
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedBool<(A > B)> result;
		};
	};

	struct Lt
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<(A < B)> result;
		};
	};

	struct Sar
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A >> B> result;
		};
	};

	struct Sal
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <int A, int B, typename dummy>
		struct call < BoxedInt<A>, BoxedInt<B>, dummy >
		{
			typedef BoxedInt<A << B> result;
		};
	};

	struct And
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <bool A, bool B, typename dummy>
		struct call < BoxedBool<A>, BoxedBool<B>, dummy >
		{
			typedef BoxedBool<A && B> result;
		};
	};

	struct Or
	{
		template <typename A, typename B, typename dummy>
		struct call;
		
		template <bool A, bool B, typename dummy>
		struct call < BoxedBool<A>, BoxedBool<B>, dummy >
		{
			typedef BoxedBool<A || B> result;
		};
	};

//Others not primitive

        struct Ash
        {
                template < typename Num, typename Shift, typename dummy1224 >
                class call
                {
			typedef typename Gt::template call< Shift, TL_core::BoxedInt<0>, TL_core::dummyType >::result call1217;

                        template < typename Cond, typename dummy1218 >
                        class if_arms1216
                        {
                        };
                        

                        template < typename dummy1219 >
                        class if_arms1216 < TL_core::True, dummy1219 >
                        {
				typedef typename Sal::template call< Num, Shift, TL_core::dummyType >::result call1220;
                        public: typedef call1220 result;
                        };
                        
                        template < typename dummy1221 >
                        class if_arms1216 < TL_core::False, dummy1221 >
                        {
				typedef typename Neg::template call< Shift, TL_core::dummyType >::result call1223;
				typedef typename Sar::template call< Num, call1223, TL_core::dummyType >::result call1222;
                        public: typedef call1222 result;
                        };
                        
			typedef typename if_arms1216<call1217, dummyType>::result if1215;
                public: typedef if1215 result;
                };
                
        };

        struct Null
        {
                template < typename Foo, typename dummy1475 >
                class call
                {
			typedef typename Equal::template call< Foo, TL_core::False, TL_core::dummyType >::result call1474;
                public: typedef call1474 result;
                };
                
        };
        
        struct Reverse
        {
                template < typename List, typename dummy1493 >
                class call
                {
                        struct lambda1477
                        {
                                template < typename, typename, typename, typename > class call;
                                

                                template < typename Argold_list1478, typename Argnew_list1479, typename Arghelper1480, typename dummy1491 >
                                class call
                                {
                                        template < typename, typename > class if_arms1482;
                                        
                                        
					typedef typename Null::template call< Argold_list1478, TL_core::dummyType >::result call1483;
                                        template < typename Cond, typename dummy1484 >
                                        class if_arms1482
                                        {
                                        };
                                        
                                        template < typename dummy1485 >
                                        class if_arms1482 < TL_core::True, dummy1485 >
                                        {
                                        public: typedef Argnew_list1479 result;
                                        };
                                        
                                        template < typename dummy1486 >
                                        class if_arms1482 < TL_core::False, dummy1486 >
                                        {
						typedef typename Cdr::template call< Argold_list1478, TL_core::dummyType >::result call1488;
						typedef typename Car::template call< Argold_list1478, TL_core::dummyType >::result call1490;
						typedef typename Cons::template call< call1490, Argnew_list1479, TL_core::dummyType >::result call1489;
						typedef typename Arghelper1480::template call< call1488, call1489, Arghelper1480, TL_core::dummyType >::result call1487;
                                        public: typedef call1487 result;
                                        };
                                        
					typedef typename if_arms1482<call1483, dummyType>::result if1481;
                                public: typedef if1481 result;
                                };
                                
                        };
                        
			typedef lambda1477 let1476;
			typedef typename let1476::template call< List, TL_core::False, let1476, TL_core::dummyType >::result call1492;
                public: typedef call1492 result;
                };
                
        };
        
        struct One_plus
        {
                template < typename N, typename dummy1495 >
                class call
                {
			typedef typename Add::template call< N, TL_core::BoxedInt<1>, TL_core::dummyType >::result call1494;
                public: typedef call1494 result;
                };
                
        };
        
        struct One_minus
        {
                template < typename N, typename dummy1497 >
                class call
                {
			typedef typename Sub::template call< N, TL_core::BoxedInt<1>, TL_core::dummyType >::result call1496;
                public: typedef call1496 result;
                };
                
        };
        
        struct Iota
        {
                template < typename N, typename dummy1515 >
                class call
                {
                        struct lambda1499
                        {
                                template < typename Argi1500, typename Argacc1501, typename Arghelper1502, typename dummy1513 >
                                class call
                                {
                                        template < typename, typename > class if_arms1504;
                                        
                                        
					typedef typename Gt::template call< Argi1500, N, TL_core::dummyType >::result call1505;
                                
                                        template < typename Cond, typename dummy1506 >
                                        class if_arms1504
                                        {
                                        };
                                        
                                        template < typename dummy1507 >
                                        class if_arms1504 < TL_core::True, dummy1507 >
                                        {
						typedef typename Reverse::template call< Argacc1501, TL_core::dummyType >::result call1508;
                                        public: typedef call1508 result;
                                        };
                                        
                                        template < typename dummy1509 >
                                        class if_arms1504 < TL_core::False, dummy1509 >
                                        {
						typedef typename One_plus::template call< Argi1500, TL_core::dummyType >::result call1511;
						typedef typename Cons::template call< Argi1500, Argacc1501, TL_core::dummyType >::result call1512;
						typedef typename Arghelper1502::template call< call1511, call1512, Arghelper1502, TL_core::dummyType >::result call1510;
                                        public: typedef call1510 result;
                                        };
                                        
					typedef typename if_arms1504<call1505, dummyType>::result if1503;
                                public: typedef if1503 result;
                                };
                                
                        };
                        
			typedef lambda1499 let1498;
			typedef typename let1498::template call< TL_core::BoxedInt<1>, TL_core::False, let1498, TL_core::dummyType >::result call1514;
                public: typedef call1514 result;
                };
                
        };
        
        struct Length
        {
                template < typename List, typename dummy1532 >
                class call
                {
                        struct lambda1517
                        {
                                template < typename Arglist1518, typename Argn1519, typename Arghelper1520, typename dummy1530 >
                                class call
                                {
					typedef typename Null::template call< Arglist1518, TL_core::dummyType >::result call1523;
                                        template < typename Cond, typename dummy1524 >
                                        class if_arms1522
                                        {
                                        };
                                        
                                        template < typename dummy1525 >
                                        class if_arms1522 < TL_core::True, dummy1525 >
                                        {
                                        public: typedef Argn1519 result;
                                        };
                                        
                                        template < typename dummy1526 >
                                        class if_arms1522 < TL_core::False, dummy1526 >
                                        {
						typedef typename Cdr::template call< Arglist1518, TL_core::dummyType >::result call1528;
						typedef typename One_plus::template call< Argn1519, TL_core::dummyType >::result call1529;
						typedef typename Arghelper1520::template call< call1528, call1529, Arghelper1520, TL_core::dummyType >::result call1527;
                                        public: typedef call1527 result;
                                        };
                                        
					typedef typename if_arms1522<call1523, dummyType>::result if1521;
                                public: typedef if1521 result;
                                };
                                
                        };
                        
			typedef lambda1517 let1516;
			typedef typename let1516::template call< List, TL_core::BoxedInt<0>, let1516, TL_core::dummyType >::result call1531;
                public: typedef call1531 result;
                };
                
        };
}

namespace TL_core_symbols
{
	typedef TL_core::False False;
	typedef TL_core::True  True;
	typedef TL_core::Nil Nil;

#define DefStd(symbol) struct symbol { typedef TL_core::symbol result; }

	DefStd(Cons);
	DefStd(Car);
	DefStd(Cdr);
	DefStd(Equal);
	DefStd(Neg);
	DefStd(Add);
	DefStd(Sub);
	DefStd(Mul);
	DefStd(Div);
	DefStd(Mod);
	DefStd(Logxor);
	DefStd(Logand);
	DefStd(Logior);
	DefStd(Lognot);
	DefStd(Not);
	DefStd(Gt);
	DefStd(Lt);
	DefStd(Sar);
	DefStd(Sal);
	DefStd(And);
	DefStd(Or);
	DefStd(Ash);
	DefStd(Null);
	DefStd(Reverse);
	DefStd(One_plus);
	DefStd(One_minus);
	DefStd(Iota);
	DefStd(Length);

}
