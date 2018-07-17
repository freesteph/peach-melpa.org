import statuses from "statuses";
import logger from "./logger";
import sanitise from "./textSanitiser";

const production = process.env.NODE_ENV === "production";
const logErrors =
  process.env.NODE_ENV === "test" ? process.env.LOG_TEST_ERRORS : true;

export default serviceName => (err, req, res, next) => {
  // eslint-disable-line no-unused-vars
  const status = err.status || err.statusCode || 500;

  if (logErrors) logger.error((err && err.stack) || err);

  if (req.traceSpan) {
    req.traceSpan.addTags({
      "ctx.headers": JSON.stringify(req.headers),
      "req.body": sanitise(JSON.stringify(req.body)),
      "req.params": sanitise(JSON.stringify(req.params)),
      error: true,
      "error.msg": err.message,
      "error.stack": err.stack,
      "error.type": err.name,
      "http.status_code": status
    });
  }

  res.status(status).json({
    status,
    stack: production ? null : err.stack,
    message: err.message || statuses[status],
    code: err.code,
    name: err.name,
    type: err.type,
    details: err.details,
    cat: `https://http.cat/${status}.jpg`,
    origin: serviceName
  });
};
