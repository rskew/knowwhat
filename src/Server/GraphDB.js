exports._enableForeignKey = function (db) {
    return function (onError, onSuccess) {
        db.run("PRAGMA foreign_keys = ON;", function (err) {
            if (err != null) {
                console.log("failed to enable foreign key constraint");
                onError();
            } else {
                console.log("enabled foreign key constraint");
                onSuccess();
            }
        });

        return function (cancelError, cancelerError, cancelerSuccess) {
            cancelerSuccess();
        };
    };
};
