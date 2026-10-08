// SPDX-License-Identifier: AGPL-3.0-or-later

import type {types} from 'cassandra-driver';

type CassandraValue =
	| string
	| number
	| bigint
	| boolean
	| Buffer
	| Date
	| types.LocalDate
	| Set<unknown>
	| Map<unknown, unknown>
	| Array<unknown>
	| Record<string, unknown>
	| null;
export type CassandraParams = Record<string, CassandraValue>;

export interface PreparedQuery<P extends CassandraParams = CassandraParams> {
	cql: string;
	params: P;
}
