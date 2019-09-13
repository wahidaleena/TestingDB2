IDENTIFICATION DIVISION.
       PROGRAM-ID.    TEST DB2.
      *SECURITY.      OPERACTION, REVISION, AND DISTRIBUTION
      *            OF THIS PROGRAM BY WRITTEN AUTHORIZATION
      *            OF THE ABOVE INSTALLACTION ONLY.
      *DATE-WRITTEN.  09/12/19.
      *DATE-COMPLETED.
      **************************CC109**********************************
       
       PROCEDURE DIVISION.

       0000-INITIALIZE-PARA.

          
        EXEC SQL
            DECLARE  FOODSTMP_S CURSOR FOR       
            SELECT   DISTINCT FD_STMP                  
            FROM     TABX X,                    
                     TABS S                      
            WHERE X.ROG          = :X-ROG        
              AND X.CORP_ITEM_CD = :MEX7-CORP-ITEM-CD
              AND X.UNIT_TYPE    = :JUI-UNIT-TYPE   
              AND X.STATUS_RUPC ¬= 'X'             
              AND X.ROG          = S.ROG        
              AND X.UNIT_TYPE    = S.UNIT_TYPE 
              AND X.UPC_MANUF    = S.UPC_MANUF
              AND X.UPC_SALES    = S.UPC_SALES
              AND X.UPC_COUNTRY  = S.UPC_COUNTRY
              AND X.UPC_SYSTEM   = S.UPC_SYSTEM
            QUERYNO 43                      


        END-EXEC.

      1000-INITIALIZE-PARA.
	      EXEC SQL
            SELECT   USERID
            INTO    RTL_USERID
            FROM     MERT RTL,
            CORO RGT
            WHERE     RTL.PA_ROG     = RGT.ROG
            AND     RTL.USERID     = :XF-USERID
            AND     RTL.TYPE       = 'R'
            AND     RGT.COUNTRY_CD = :PRX-COUNTRY-CD
            AND (RGT.ROG =
            CASE WHEN :WRG-WRG01 <> ' '
            THEN :WRG-WRG01
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG02 <> ' '
            THEN :WRG-WRG02
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG03 <> ' '
            THEN :WRG-WRG03
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG04 <> ' '
            THEN :WRG-WRG04
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG05 <> ' '
            THEN :WRG-WRG05
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG06 <> ' '
            THEN :WRG-WRG06
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG07 <> ' '
            THEN :WRG-WRG07
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG08 <> ' '
            THEN :WRG-WRG08
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG09 <> ' '
            THEN :WRG-WRG09
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG10 <> ' '
            THEN :WRG-WRG10
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG11 <> ' '
            THEN :WRG-WRG11
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG12 <> ' '
            THEN :WRG-WRG12
            ELSE '    '
            END
            OR  RGT.ROG =
            CASE WHEN :WRG-WRG01 =  '    '
            AND :WRG-WRG02 =  '    '
            AND :WRG-WRG03 =  '    '
            AND :WRG-WRG04 =  '    '
            AND :WRG-WRG05 =  '    '
            AND :WRG-WRG06 =  '    '
            AND :WRG-WRG07 =  '    '
            AND :WRG-WRG08 =  '    '
            AND :WRG-WRG09 =  '    '
            AND :WRG-WRG10 =  '    '
            AND :WRG-WRG11 =  '    '
            AND :WRG-WRG12 =  '    '
            THEN RTL.PA_ROG
            END)
            FETCH FIRST ROW ONLY
            QUERYNO 39



        END-EXEC.
		
	  2000-INITIALIZE-PARA.
	   EXEC SQL
            DELETE
                FROM  COUPON
                WHERE ROG         = :X-ROG
                AND UPC_MANUF   = :HV-UPC-MANUF
                AND UPC_SALES   = :HV-UPC-SALES
                AND UPC_COUNTRY = :HV-UPC-COUNTRY
                AND UPC_SYSTEM  = :HV-UPC-SYSTEM
                AND POS_PROCESSED_IND IN (' ', 'F')
                AND PACS_ADPL_SEQ_NUM IN
                (SELECT PACS_ADPL_SEQ_NUM
                FROM   PENDING
                WHERE  ROG = :X-ROG
                AND  CORP_ITEM_CD = :MEX7-CORP-ITEM-CD
                AND  UNIT_TYPE    = :HV-UNIT-TYPE
                AND  AD_SELECT    = :HV-AD-SELECT)
                QUERYNO  74

     END-EXEC.

      

      ******************************************************************
      * TABLE INSERT                                                   *
      ******************************************************************
       4000-INSERT-TABLE.

       EXEC SQL
        SELECT  COALESCE(COUNT(*),0)
            INTO    :HV-UPCCNT
            FROM    RF X
            WHERE   CORP_ITEM_CD = :X-CORP-ITEM-CD
              AND NOT EXISTS(SELECT 1
                       FROM  SC C
                       WHERE C.CORP      = :HV-CORP
                         AND C.UPC_MANUF   = X.UPC_MANUF
                         AND C.UPC_SALES   = X.UPC_SALES
                         AND C.UPC_COUNTRY = X.UPC_COUNTRY
                         AND C.UPC_SYSTEM  = X.UPC_SYSTEM)
          QUERYNO 17

       END-EXEC.
       
       5000-INSERT-TABLE.

       EXEC SQL
        OPEN READNEXT
       END-EXEC.
       
       6000-INSERT-TABLE.

       EXEC SQL
        CLOSE READNEXT
       END-EXEC.
       
       6000-FINAL-COUNT.
          CLOSE INFILE
                OUT.
          DISPLAY "-----------------------------------------------------".
