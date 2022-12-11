with non_null as (
  select distinct lower(drugname) as drugname1,drughiclseqno
  from `icu-data-260103.eicu_crd.medication`
  where drugname is not null and drughiclseqno is not null
  order by drughiclseqno
),
non_null1 as (
  select drughiclseqno,STRING_AGG(drugname1 ORDER BY drugname1) AS drugname1
  from non_null
  group by drughiclseqno
)
select t1.*,t2.drugname1
from `icu-data-260103.eicu_crd.medication` t1
left join non_null1  t2
on t1.drughiclseqno = t2.drughiclseqno
where drugname is null and t1.drughiclseqno is not null
union distinct
select *, null as drugname1
from `icu-data-260103.eicu_crd.medication`
where drugname is not null 