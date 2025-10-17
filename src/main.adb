-- A skeleton of an ADA program for an assignment in programming languages
 
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
 
procedure Simulation is
 
    ----GLOBAL VARIABLES---
 
    Ilosc_kucharzy  : constant Integer := 5;
    Ilosc_Stolow : constant Integer := 3;
    Ilosc_zjadaczy  : constant Integer := 2;
 
    subtype Typ_Kucharza is Integer range 1 .. Ilosc_kucharzy;
    subtype Typ_Stolu is Integer range 1 .. Ilosc_Stolow;
    subtype Typ_Zjadacza is Integer range 1 .. Ilosc_zjadaczy;
 
    --each Producer is assigned a Product that it produces
    Nazwa_Dania  : constant array (Typ_Kucharza) of String (1 .. 6) :=
       ("Danie1", "Danie2", "Danie3", "Danie4", "Danie5");
    --Assembly is a collection of products
    Nazwa_Stolu : constant array (Typ_Stolu) of String (1 .. 5) :=
       ("Stol1", "Stol2", "Stol3");
 
    task type Szalony_Kelner is
        entry Start;
    end Szalony_Kelner;

    ----TASK DECLARATIONS----
 
    -- Producer produces determined product
    task type Kucharz is
        entry Start (Danie : in Typ_Kucharza; Czas_Gotowania : in Integer);
    end Kucharz;
 
    -- Consumer gets an arbitrary assembly of several products from the buffer
    -- but he/she orders it randomly
    task type Zjadacz is
        entry Start (Numer_Zjadacza : in Typ_Zjadacza; Czas_Zjadania : in Integer);
    end Zjadacz;
 
    -- Buffer receives products from Producers and delivers Assemblies to Consumers
    task type Stolik is
        -- Accept a product to the storage (provided there is a room for it)
        entry Take (Danie : in Typ_Kucharza; Number : in Integer);
        -- Deliver an assembly (provided there are enough products for it)
        entry Deliver (Stol : in Typ_Stolu; Number : out Integer);
        -- Furious worker robi problemy
        entry Quarrel_In_Storage;
    end Stolik;
 
    FW : Szalony_Kelner;
    P : array (1 .. Ilosc_kucharzy) of Kucharz;
    K : array (1 .. Ilosc_zjadaczy) of Zjadacz;
    B : Stolik;
 
    ----TASK DEFINITIONS----
 
    task body Szalony_Kelner is
        subtype fury_level is Integer range 0 .. 10;
        package Random_Fury is new Ada.Numerics.Discrete_Random (fury_level);
        G            : Random_Fury.Generator;
        fury_trigger : Integer;
        fury         : Integer;
        Time  : Duration;
    begin
        accept Start
        do
            Random_Fury.Reset (G);
            fury_trigger := 8;
            fury         := 0;
            Time := Duration(0.5);
        end Start;
        Put_Line ("Kelner wychodzi na zer");
        loop
            delay Time;
            fury := Random_Fury.Random (G);
            if fury > fury_trigger then
                B.Quarrel_In_Storage;
            end if;
        end loop;
    end Szalony_Kelner;
 
    --Producer--
 
    task body Kucharz is
        subtype Czas_Gotowania_Rozmiar is Integer range 1 .. 3;
        package Random_Production is new Ada.Numerics.Discrete_Random (Czas_Gotowania_Rozmiar);
        --  random number generator
        G                    : Random_Production.Generator;
        Producer_Type_Number : Integer;
        Product_Number       : Integer;
        Production           : Integer;
        Random_Time          : Duration;
    begin
        accept Start (Danie : in Typ_Kucharza; Czas_Gotowania : in Integer)
        do
            --  start random number generator
            Random_Production.Reset (G);
            Product_Number       := 1;
            Producer_Type_Number := Danie;
            Production           := Czas_Gotowania;
        end Start;
        Put_Line
           (ESC & "[93m" & "P: Started producer of " &
            Nazwa_Dania (Producer_Type_Number) & ESC & "[0m");
        loop
            Random_Time := Duration (Random_Production.Random (G));
            delay Random_Time;
            Put_Line
               (ESC & "[93m" & "P: Produced product " &
                Nazwa_Dania (Producer_Type_Number) & " number " &
                Integer'Image (Product_Number) & ESC & "[0m");
            -- Accept for storage
 
            -- We try to deliver product 3b
            select
                B.Take (Producer_Type_Number, Product_Number);
                Put_Line
                   (ESC & "[93m" & "P: Delivered " &
                    Nazwa_Dania (Producer_Type_Number) & " number " &
                    Integer'Image (Product_Number) & ESC & "[0m");
                Product_Number := Product_Number + 1;
            or
                delay Random_Time;
                Put_Line
                   (ESC & "[93m" &
                    "P: TIMEOUT - Buffer not responding, lost " &
                    Nazwa_Dania (Producer_Type_Number) & " number " &
                    Integer'Image (Product_Number) & ESC & "[0m");
                Product_Number := Product_Number + 1;
 
            end select;
        end loop;
    end Kucharz;
 
    --Consumer--
 
    task body Zjadacz is
        subtype Czas_Zjadania_Rozmiar is Integer range 4 .. 8;
        package Random_Consumption is new Ada.Numerics.Discrete_Random
           (Czas_Zjadania_Rozmiar);
 
        --each Consumer takes any (random) Assembly from the Buffer
        package Random_Assembly is new Ada.Numerics.Discrete_Random
           (Typ_Stolu);
 
        G               : Random_Consumption.Generator;
        GA              : Random_Assembly.Generator;
        Consumer_Nb     : Typ_Zjadacza;
        Assembly_Number : Integer;
        Consumption     : Integer;
        Typ_Stolu   : Integer;
        Consumer_Name   :
           constant array (1 .. Ilosc_zjadaczy) of String (1 .. 8) :=
           ("Zjadacz1", "Zjadacz2");
        Retry_Delay     : constant Duration := 1.0;
    begin
        accept Start(Numer_Zjadacza : in Typ_Zjadacza; Czas_Zjadania : in Integer)
        do
            Random_Consumption.Reset (G);
            Random_Assembly.Reset (GA);
            Consumer_Nb := Numer_Zjadacza;
            Consumption := Czas_Zjadania;
        end Start;
        Put_Line
           (ESC & "[96m" & "C: Started consumer " &
            Consumer_Name (Consumer_Nb) & ESC & "[0m");
        loop
            delay Duration
               (Random_Consumption.Random (G)); --  simulate consumption
            Typ_Stolu := Random_Assembly.Random (GA);
 
            -- 3a making sure that consument can't get assembly nr 0
            declare
                Succes       : Boolean          := False;
                Attempts     : Integer          := 0;
                Max_Attempts : constant Integer := 3;
            begin
                while not Succes and Attempts < Max_Attempts loop
                    -- take an assembly for consumption
                    B.Deliver (Typ_Stolu, Assembly_Number);
                    if Assembly_Number /= 0 then
                        Succes := True;
                        Put_Line
                           (ESC & "[96m" & "C: " &
                            Consumer_Name (Consumer_Nb) & " takes assembly " &
                            Nazwa_Stolu (Typ_Stolu) & " number " &
                            Integer'Image (Assembly_Number) & ESC & "[0m");
                    else
                        -- if assembly wasn't valid
                        Attempts := Attempts + 1;
                        if Attempts < Max_Attempts then
                            Put_Line
                               (ESC & "[96m" & "C: " &
                                Consumer_Name (Consumer_Nb) &
                                " cannot get assembly " &
                                Nazwa_Stolu (Typ_Stolu) &
                                ", retrying..." & ESC & "[0m");
                            delay Retry_Delay;
                        else
                            Put_Line
                               (ESC & "[96m" & "C: " &
                                Consumer_Name (Consumer_Nb) &
                                " giving up on assembly " &
                                Nazwa_Stolu (Typ_Stolu) & " after " &
                                Integer'Image (Max_Attempts) & " attempts" &
                                ESC & "[0m");
                        end if;
                    end if;
                end loop;
 
            end;
        end loop;
    end Zjadacz;

    --Buffer--
 
    task body Stolik is
        Storage_Capacity : constant Integer := 30;
        type Storage_type is array (Typ_Kucharza) of Integer;
        Storage              : Storage_type := (0, 0, 0, 0, 0);
        Assembly_Content : array (Typ_Stolu, Typ_Kucharza) of Integer :=
           ((2, 1, 2, 0, 2), (1, 2, 0, 1, 0), (3, 2, 2, 0, 1));
        Max_Assembly_Content : array (Typ_Kucharza) of Integer;
        Assembly_Number      : array (Typ_Stolu) of Integer := (1, 1, 1);
        In_Storage           : Integer := 0;
 
        Timeout : constant Duration := 2.0; -- duration of waiting for call
 
        procedure Setup_Variables is
        begin
            for W in Typ_Kucharza loop
                Max_Assembly_Content (W) := 0;
                for Z in Typ_Stolu loop
                    if Assembly_Content (Z, W) > Max_Assembly_Content (W) then
                        Max_Assembly_Content (W) := Assembly_Content (Z, W);
                    end if;
                end loop;
            end loop;
        end Setup_Variables;
 
        function Can_Accept (Danie : Typ_Kucharza) return Boolean is
            Usage_Ratio        : Float   :=
               Float (In_Storage) / Float (Storage_Capacity);
            Avg_Storage        : Float   :=
               Float (In_Storage) / Float (Ilosc_kucharzy);
            Product_Needed     : Boolean := False;
            Product_Bottleneck : Boolean := False;
 
           
        begin
            -- Bufor pelny
            if In_Storage >= Storage_Capacity then
                return False;
            end if;
 
            -- Jezeli bufor pelny (powyzej 90%) - przyjmujemy najmniej liczne produkty
            if Usage_Ratio > 0.9 then
                declare
                    Min_Count : Integer := Storage (1);
                begin
                    -- Znajdź minimalny poziom zapasu wśród wszystkich produktów
                    for P in Typ_Kucharza loop
                        if Storage (P) < Min_Count then
                            Min_Count := Storage (P);
                        end if;
                    end loop;
 
                    -- Przyjmij tylko produkty o najmniejszym stanie magazynowym
                    if Storage (Danie) = Min_Count then
                        return True;
                    else
                        return False;
                    end if;
                end;
            end if;
 
            -- Jezeli bufor bardzo pelny (powyzej 80%) - przyjmujemy tylko produkty potrzebne do dokonczenia zestawu
            if Usage_Ratio > 0.8 then
                for A in Typ_Stolu loop
                    declare
                        Missing : Boolean := False;
                    begin
                        for P in Typ_Kucharza loop
                            if Storage (P) < Assembly_Content (A, P) then
                                Missing := True;
                            end if;
                        end loop;
 
                        if Missing and then Assembly_Content (A, Danie) > 0
                        then
                            Product_Needed := True;
                            exit;
                        end if;
                    end;
                end loop;
                return Product_Needed;
            end if;
 
            -- Jesli bufor srednio zapelniony (50–80%) - unikamy nadprodukcji jednego produktu
            if Usage_Ratio > 0.5
               and then Float (Storage (Danie)) > 1.5 * Avg_Storage
            then
                return False;
            end if;
 
            -- Jesli bufor malo zapelniony (<50%) – wspieramy produkty, ktore są w deficycie wzgledem innych
            declare
                Min_Count : Integer := Storage (1);
                Max_Count : Integer := Storage (1);
            begin
                for P in Typ_Kucharza loop
                    if Storage (P) < Min_Count then
                        Min_Count := Storage (P);
                    end if;
                    if Storage (P) > Max_Count then
                        Max_Count := Storage (P);
                    end if;
                end loop;
 
                if Storage (Danie) = Min_Count then
                    Product_Bottleneck := True;
                end if;
 
                if Product_Bottleneck then
                    return True;
                end if;
            end;
 
            return True;
        end Can_Accept;
 
        function Can_Deliver (Stol : Typ_Stolu) return Boolean is
            Usage_Ratio     : Float   :=
               Float (In_Storage) / Float (Storage_Capacity);
            Enough          : Boolean := True;
            Total_Required  : Integer := 0;
            Min_After_Deliv : Integer := Integer'Last;
        begin
            -- Mozna zrobic zestaw
            for P in Typ_Kucharza loop
                if Storage (P) < Assembly_Content (Stol, P) then
                    Enough := False;
                    exit;
                end if;
                Total_Required :=
                   Total_Required + Assembly_Content (Stol, P);
            end loop;
 
            if not Enough then
                return False;
            end if;
 
            -- Minimalny poziom zapasu po dostawie (chroni przed zaglodzeniem)
            for P in Typ_Kucharza loop
                declare
                    After : Integer :=
                       Storage (P) - Assembly_Content (Stol, P);
                begin
                    if After < Min_After_Deliv then
                        Min_After_Deliv := After;
                    end if;
                end;
            end loop;
 
            -- Jezeli bufor bardzo pusty ( ponizej 30%), nie wydawaj jeśli coś spadłoby poniżej 2
            if Usage_Ratio < 0.3 and then Min_After_Deliv < 2 then
                return False;
            end if;
 
            -- Jezeli bufor bardzo pelny (powyzej 80%) - preferuj duze zestawy
            if Usage_Ratio > 0.8 and then Total_Required < 3 then
                return False;
            end if;
 
            return True;
        end Can_Deliver;
 
        -- Wywalanie polowy zasobow
        procedure Throwing_Products is
        begin
            for P in Typ_Kucharza loop
                Storage(P) := Integer(Float'Ceiling(Float(Storage(P)) / 2.0));
            end loop;
            In_Storage := 0;
            for P in Typ_Kucharza loop
                In_Storage := In_Storage + Storage(P);
            end loop;
            Put_Line (ESC & "[91m" & "After the quarrel, half of the products were lost!" & ESC & "[0m");
        end Throwing_Products;
 
        procedure Storage_Contents is
        begin
            for W in Typ_Kucharza loop
                Put_Line
                   ("|   Storage contents: " & Integer'Image (Storage (W)) &
                    " " & Nazwa_Dania (W));
            end loop;
            Put_Line
               ("|   Number of products in storage: " &
                Integer'Image (In_Storage));
        end Storage_Contents;
 
    begin
        Put_Line (ESC & "[91m" & "B: Buffer started" & ESC & "[0m");
        Setup_Variables;
 
        declare
            Full_Buffer_Count : Integer          := 0;
            Max_Full_Cycles   : constant Integer := 5; -- po tylu cyklach czyscimy najwiekszy element
        begin
            loop
                select
                    accept Take
                       (Danie : in Typ_Kucharza; Number : in Integer)
                    do
                        if Can_Accept (Danie) then
                            Put_Line
                               (ESC & "[91m" & "B: Accepted product " &
                                Nazwa_Dania (Danie) & " number " &
                                Integer'Image (Number) & ESC & "[0m");
                            Storage (Danie) := Storage (Danie) + 1;
                            In_Storage        := In_Storage + 1;
                            Full_Buffer_Count := 0; -- reset licznika
 
                        else
                            Put_Line
                               (ESC & "[91m" & "B: Rejected product " &
                                Nazwa_Dania (Danie) & " number " &
                                Integer'Image (Number) & ESC & "[0m");
                            if In_Storage >= Storage_Capacity then
                                Full_Buffer_Count := Full_Buffer_Count + 1;
                                if Full_Buffer_Count >= Max_Full_Cycles then
                                    declare
                                        Max_P     : Typ_Kucharza := 1;
                                        Max_Value : Integer := Storage (1);
                                    begin
                                        -- znajdujemy produkt z najwieksza iloscia w magazynie
                                        for P in Typ_Kucharza loop
                                            if Storage (P) > Max_Value then
                                                Max_Value := Storage (P);
                                                Max_P     := P;
                                            end if;
                                        end loop;
 
                                        -- usun 1 sztuke tego produktu, by odblokowac system
                                        -- UWAGA!!!! tylko w ostatecznosci
                                        if Max_Value > 0 then
                                            Storage (Max_P) :=
                                               Storage (Max_P) - 1;
                                            In_Storage      := In_Storage - 1;
                                            Put_Line
                                               (ESC & "[91m" &
                                                "B: Buffer full too long – removed one " &
                                                Nazwa_Dania (Max_P) & ESC &
                                                "[0m");
                                        end if;
 
                                        Full_Buffer_Count := 0;
                                    end;
                                end if;
                            end if;
                        end if;
                    end Take;
                    Storage_Contents;
                or
                    accept Deliver
                       (Stol : in Typ_Stolu; Number : out Integer)
                    do
                        if Can_Deliver (Stol) then
                            Put_Line
                               (ESC & "[91m" & "B: Delivered assembly " &
                                Nazwa_Stolu (Stol) & " number " &
                                Integer'Image (Assembly_Number (Stol)) &
                                ESC & "[0m");
                            for W in Typ_Kucharza loop
                                Storage (W) :=
                                   Storage (W) -
                                   Assembly_Content (Stol, W);
                                In_Storage  :=
                                   In_Storage - Assembly_Content (Stol, W);
                            end loop;
                            Number := Assembly_Number (Stol);
                            Assembly_Number (Stol) :=
                               Assembly_Number (Stol) + 1;
                            Full_Buffer_Count := 0; -- reset po udanym wydaniu
 
                        else
                            Put_Line
                               (ESC & "[91m" &
                                "B: Lacking products for assembly " &
                                Nazwa_Stolu (Stol) & ESC & "[0m");
                            Number := 0;
                        end if;
                    end Deliver;
                    Storage_Contents;
                or
                    accept Quarrel_In_Storage do
                        Put_Line (ESC & "[91m" & "A quarrel in the storage!" & ESC & "[0m");
                        Throwing_Products;
                    end Quarrel_In_Storage;
                end select;
            end loop;
        end;
    end Stolik;

    ---"MAIN" FOR SIMULATION---
begin
    FW.Start;
    for I in 1 .. Ilosc_kucharzy loop
        P (I).Start (I, 10);
    end loop;
    
    for J in 1 .. Ilosc_zjadaczy loop
        K (J).Start (J, 12);
    end loop;
end Simulation;