--SELECT *,
----UPDATE A SET [Address] = 
--REPLACE([Address], ' корп.-,', ',')
--FROM taxes_clients_2014_addresses A
--WHERE CHARINDEX(' корп.-,', [Address]) > 0

SELECT C.[client_id], C.[code], C.[client_name], C.[jur_adresse], I.[Count]
FROM
(
  SELECT [client_name], [address], [count] = COUNT(*) 
  FROM taxes_clients_2014_addresses WITH (NOLOCK)
  --WHERE client_id in 
  --      (SELECT DISTINCT cl.client_base_id
  --        FROM          client AS cl WITH (NOLOCK) INNER JOIN
  --                               dogovor AS d WITH (NOLOCK) ON cl.client_id = d.client_id
  --        WHERE      (d.dogovor_type = 643))
  GROUP BY [client_name], [address]
  HAVING COUNT(*) > 1
) I
LEFT JOIN
(
  SELECT [client_name], [address] = jur_adresse, [count] = COUNT(*) 
  FROM client WITH (NOLOCK)
  GROUP BY [client_name], jur_adresse
  HAVING COUNT(*) > 1
) CC ON I.[client_name] = CC.[client_name] AND I.[address] = CC.[address]

INNER JOIN taxes_clients_2014_addresses A WITH (NOLOCK) ON I.[client_name] = A.[client_name] AND I.[address] = A.[address]
INNER JOIN client C WITH (NOLOCK) ON A.client_id = C.client_id

WHERE I.[address] <> ''
AND CC.[client_name] IS NULL
ORDER BY 3, 4

-- SELECT jur_adresse FROM client WITH (NOLOCK) WHERE client_name = 'Айбедуллов Альберт Феритович'
--SELECT * FROM client WITH (NOLOCK) WHERE client_id = 141540