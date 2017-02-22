import Elephant from '@musicglue/elephant';
import methodMetrics from '@musicglue/mg-express/lib/methodMetrics';
import { db, queryFile } from '../database';

const upsertQuery = queryFile('models/digital/upsert');
const cleanSubDigitalsQuery = queryFile('models/digital/cleanSubDigitals');
const upsertValues = '$(id), $(parentId), $(updatedAt)';
const getParams = payload => ({
  id: payload.id,
  parentId: (payload.parentId || null),
  updatedAt: payload.updatedAt,
});

class Digital extends Elephant {
  upsert(payloads, options) {
    const insertables = Array.isArray(payloads) ? payloads : [payloads];
    if (!insertables.length) return Promise.resolve([]);

    const inserts = new Elephant.Inserts(upsertValues, insertables.map(getParams));
    return this.none(this.write, upsertQuery, [inserts], options);
  }

  cleanSubDigitals(payload, options) {
    return this.any(this.write, cleanSubDigitalsQuery, payload, options);
  }
}

export default methodMetrics(
  new Digital(db), 'elephant', 'digital', ['upsert', 'cleanSubDigitals']
);
