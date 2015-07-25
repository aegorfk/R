#Создание коллекции
> mongo
> db.apps.drop()
> db.createCollection("apps", {autoIndexId: true} )


tmp = mongo.find.one(mongo, ns = "test.apps")
tmp

find_all = mongo.find.all(mongo, ns = "test.apps")


query <- mongo.bson.from.list(list(date = as.POSIXct(update, tz='MSK')))