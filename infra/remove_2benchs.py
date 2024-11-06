import json

data = []
with open("points_modified.json", "r") as points:
    for line in points:
        record = json.loads(line)
        #if record['exprs'] != ['(assert (TRUE))', '(* (fmod (exp x) (sqrt (cos x))) (exp (neg x)))'] and \
        #  record['exprs'] != ['(assert (TRUE))', '(* (exp (neg w)) (pow l (exp w)))']:
        #  data.append(record)
        data.append(record)
 
print(len(data))       
#with open('points_modified.json', 'w') as outfile:
#    for entry in data:
#        json.dump(entry, outfile)
#        outfile.write('\n')
      
 
        

