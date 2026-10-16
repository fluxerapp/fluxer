import assert from 'node:assert/strict';
import {test} from 'node:test';
import type {OpenAPIDocument} from '@fluxer/openapi/src/OpenAPITypes';
import type {MarkdownPage} from './DocsSource.ts';
import {type SchemaVerification, verifyPages} from './VerifyDocsSchemas.ts';

const SNOWFLAKE = {$ref: '#/components/schemas/SnowflakeType'};
const WIDGET = {$ref: '#/components/schemas/Widget'};

const spec = {
	openapi: '3.1.0',
	info: {title: 'fixture', version: '1'},
	paths: {
		'/widgets/{widget_id}': {
			patch: {
				operationId: 'update_widget',
				tags: [],
				requestBody: {content: {'application/json': {schema: {$ref: '#/components/schemas/WidgetUpdate'}}}},
				responses: {'200': {description: 'ok', content: {'application/json': {schema: WIDGET}}}},
			},
		},
		'/widgets': {
			get: {
				operationId: 'list_widgets',
				tags: [],
				parameters: [
					{name: 'limit', in: 'query', required: false, schema: {type: 'integer'}},
					{name: 'owner_id', in: 'query', required: false, schema: SNOWFLAKE},
				],
				responses: {
					'200': {
						description: 'ok',
						content: {
							'application/json': {
								schema: {
									type: 'object',
									properties: {widgets: {type: 'array', items: WIDGET}, total: {type: 'integer'}},
									required: ['widgets', 'total'],
									additionalProperties: false,
								},
							},
						},
					},
				},
			},
			post: {
				operationId: 'create_widget',
				tags: [],
				requestBody: {
					content: {
						'application/json': {
							schema: {
								oneOf: [
									{
										type: 'object',
										properties: {kind: {type: 'string', const: 'alpha'}, alpha: {type: 'string'}},
										required: ['kind', 'alpha'],
									},
									{
										type: 'object',
										properties: {kind: {type: 'string', const: 'beta'}, beta: {type: 'integer'}},
										required: ['kind', 'beta'],
									},
								],
							},
						},
					},
				},
				responses: {'200': {description: 'ok', content: {'application/json': {schema: WIDGET}}}},
			},
		},
		'/widgets/positions': {
			put: {
				operationId: 'move_widgets',
				tags: [],
				requestBody: {
					content: {
						'application/json': {schema: {type: 'array', items: {$ref: '#/components/schemas/WidgetPosition'}}},
					},
				},
				responses: {'204': {description: 'empty'}},
			},
		},
		'/owners': {
			get: {
				operationId: 'list_owners',
				tags: [],
				responses: {
					'200': {
						description: 'ok',
						content: {
							'application/json': {
								schema: {
									type: 'object',
									additionalProperties: {anyOf: [{$ref: '#/components/schemas/Owner'}, {type: 'null'}]},
								},
							},
						},
					},
				},
			},
		},
		'/gadgets': {
			post: {
				operationId: 'create_gadget',
				tags: [],
				requestBody: {content: {'application/json': {schema: {$ref: '#/components/schemas/GadgetCreate'}}}},
				responses: {'204': {description: 'empty'}},
			},
		},
	},
	components: {
		schemas: {
			SnowflakeType: {
				anyOf: [{type: 'string'}, {type: 'integer'}],
				format: 'snowflake',
			},
			Widget: {
				type: 'object',
				properties: {
					id: SNOWFLAKE,
					name: {type: 'string'},
					value: {anyOf: [{type: 'string'}, {type: 'boolean'}]},
					part: {anyOf: [{$ref: '#/components/schemas/Part'}, {type: 'null'}]},
					labels: {type: 'object', additionalProperties: {$ref: '#/components/schemas/Label'}},
					shape: {anyOf: [{$ref: '#/components/schemas/Circle'}, {$ref: '#/components/schemas/Square'}]},
					legacy: {type: 'boolean', description: 'Deprecated. Always false.'},
				},
				required: ['id', 'name', 'value', 'part'],
				additionalProperties: false,
			},
			Part: {
				type: 'object',
				properties: {part_id: SNOWFLAKE, weight: {type: 'number'}, unused: {type: 'string'}},
				required: ['part_id', 'weight'],
				additionalProperties: false,
			},
			Label: {
				type: 'object',
				properties: {text: {type: 'string'}, color: {type: 'string'}},
				required: ['text'],
				additionalProperties: false,
			},
			Owner: {
				type: 'object',
				properties: {owner_id: SNOWFLAKE, name: {type: 'string'}},
				required: ['owner_id', 'name'],
				additionalProperties: false,
			},
			Circle: {
				type: 'object',
				properties: {kind: {type: 'string'}, radius: {type: 'number'}},
				required: ['kind', 'radius'],
				additionalProperties: false,
			},
			Square: {
				type: 'object',
				properties: {kind: {type: 'string'}, side: {type: 'number'}},
				required: ['kind', 'side'],
				additionalProperties: false,
			},
			WidgetPosition: {
				type: 'object',
				properties: {id: SNOWFLAKE, position: {type: 'integer'}},
				required: ['id'],
				additionalProperties: false,
			},
			WidgetUpdate: {
				type: 'object',
				properties: {name: {type: 'string'}, owner_id: SNOWFLAKE, size: {type: 'integer'}},
				required: ['name'],
				additionalProperties: false,
			},
			GadgetCreate: {
				type: 'object',
				properties: {label: {type: 'string'}, color: {type: 'string'}},
				required: ['label'],
				additionalProperties: false,
			},
		},
		securitySchemes: {},
	},
} as unknown as OpenAPIDocument;

const emptySpec = {
	openapi: '3.1.0',
	info: {title: 'empty', version: '1'},
	paths: {},
	components: {schemas: {}, securitySchemes: {}},
} as OpenAPIDocument;

const PAGE = `---
title: Widgets
---

import RouteHeader from '@/components/RouteHeader.astro';

## Widget object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The ID of the widget |
| name | string | The name |
| value | boolean | The value |
| part | ?[part](#part-object) object | The part |
| labels? | map[string, [label](#label-object) object] | The labels by key |
| shape? | [circle](#circle-object) object \\| [square](#square-object) object | The shape |
| legacy | boolean | Always false |
| shown<sup>1</sup> | boolean | Whether the widget is shown |

<sup>1</sup> Emitted on every response although the response schema does not declare it

### Part object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| part_id | snowflake | The ID of the part |
| weight | float | The weight |

The response schema also declares \`unused\`, which a part never has.

### Label object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| text | string | The text |
| color? | string | The colour |

### Circle object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| kind | string | \`circle\` |
| radius | float | The radius |

### Square object

#### Structure

| Field | Type | Description |
| --- | --- | --- |
| kind | string | \`square\` |
| side | float | The side length |

## Widget position object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The ID of the widget |
| position? | integer | The new position |

## Owner object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| owner_id | snowflake | The ID of the owner |
| name | string | The name |

## Owners by ID object

Each property name is the ID of one owner.

### Structure

| Field | Type | Description |
| --- | --- | --- |
| &#123;owner_id&#125; | ?[owner](#owner-object) object | The owner, or null |

## Gadget creation object

### Structure

| Field | Type | Description |
| --- | --- | --- |
| label | string | The label |
| color? | string | The colour |

## Update widget

<RouteHeader method="PATCH" path="/v1/widgets/{widget_id}" />

### JSON body

| Field | Type | Description |
| --- | --- | --- |
| name | string | The new name |
| owner_id? | snowflake | The new owner |
| size? | integer | The new size |

### Response

| Status | Body | Condition |
| --- | --- | --- |
| 200 | [widget](#widget-object) object | The widget was updated |

## List widgets

<RouteHeader method="GET" path="/v1/widgets" />

### Query parameters

| Field | Type | Description |
| --- | --- | --- |
| limit? | integer | The page size |
| owner_id? | snowflake | Restrict to one owner |

### Response body

| Field | Type | Description |
| --- | --- | --- |
| widgets | array[[widget](#widget-object) object] | The widgets |
| total | integer | Every matching widget |

### Response

| Status | Body | Condition |
| --- | --- | --- |
| 200 | response body | The page was returned |

## Create widget

<RouteHeader method="POST" path="/v1/widgets" />

### JSON body

#### Alpha structure

| Field | Type | Description |
| --- | --- | --- |
| kind | string | \`alpha\` |
| alpha | string | The alpha value |

#### Beta structure

| Field | Type | Description |
| --- | --- | --- |
| kind | string | \`beta\` |
| beta | integer | The beta value |

### Response

| Status | Body | Condition |
| --- | --- | --- |
| 200 | [widget](#widget-object) object | The widget was created |

## Move widgets

<RouteHeader method="PUT" path="/v1/widgets/positions" />

### JSON body

The body is an array of [widget position objects](#widget-position-object).

### Response

| Status | Body | Condition |
| --- | --- | --- |
| 204 | empty | The widgets were moved |

## List owners

<RouteHeader method="GET" path="/v1/owners" />

### Response

| Status | Body | Condition |
| --- | --- | --- |
| 200 | [owners by ID](#owners-by-id-object) object | The owners were returned |

## Create gadget

<RouteHeader method="POST" path="/v1/gadgets" />

### JSON body

The body is a [gadget creation object](#gadget-creation-object).

### Response

| Status | Body | Condition |
| --- | --- | --- |
| 204 | empty | The gadget was created |
`;

function page(source: string): MarkdownPage {
	return {
		file: '/fixture/http-api/widgets.mdx',
		relativePath: 'http-api/widgets.mdx',
		source,
		lines: source.split('\n'),
	};
}

function verify(source: string): SchemaVerification {
	return verifyPages([page(source)], spec, emptySpec);
}

function mutate(from: string, to: string): string {
	assert.ok(PAGE.includes(from), `fixture text not found: ${from}`);
	return PAGE.replace(from, to);
}

function findings(result: SchemaVerification): Array<string> {
	return result.mismatches.map((mismatch) => `${mismatch.kind} ${mismatch.operation} ${mismatch.detail}`);
}

test('the clean fixture passes and every check runs', () => {
	const result = verify(PAGE);
	assert.deepEqual(findings(result), []);
	assert.deepEqual(result.optionalityAdvisories, []);
	assert.equal(result.counters.bodyTables, 2);
	assert.equal(result.counters.bodiesByReference, 2);
	assert.equal(result.counters.unionBodies, 1);
	assert.equal(result.counters.queryTables, 1);
	assert.equal(result.counters.responseTables, 1);
	assert.equal(result.counters.responseObjects, 3);
	assert.ok(result.counters.nestedObjects >= 2);
	assert.equal(result.counters.unlinkedObjects, 0);
	assert.ok(result.counters.typesCompared > 10);
	assert.equal(result.counters.optionalityCompared, 11);
});

test('a union body field missing from every variant table fails', () => {
	const result = verify(mutate('| beta | integer | The beta value |\n', ''));
	assert.deepEqual(findings(result), ['body-missing POST /widgets beta']);
});

test('a field documented in one variant table but missing from another fails', () => {
	const result = verify(mutate('| kind | string | `beta` |\n', ''));
	assert.deepEqual(findings(result), ['body-missing POST /widgets kind (variant 2)']);
});

test('a body documented by reference is compared with the object section', () => {
	const result = verify(mutate('| label | string | The label |', '| title | string | The label |'));
	assert.deepEqual(findings(result), ['body-extra POST /gadgets title', 'body-missing POST /gadgets label']);
});

test('a nested object linked from a field type is compared with its schema', () => {
	const result = verify(mutate('| weight | float | The weight |', '| mass | float | The weight |'));
	assert.deepEqual(findings(result), [
		'object-missing PATCH /widgets/{} (http-api/widgets#widget-object).part (http-api/widgets#part-object).weight',
		'object-extra PATCH /widgets/{} (http-api/widgets#part-object).mass',
	]);
});

test('a query row is not found in another route or table of the page', () => {
	const result = verify(mutate('| owner_id? | snowflake | Restrict to one owner |\n', ''));
	assert.deepEqual(findings(result), ['query-missing GET /widgets owner_id']);
});

test('a snowflake is typed string, so a snowflake documented as integer fails', () => {
	const result = verify(mutate('| id | snowflake | The ID of the widget |', '| id | integer | The ID of the widget |'));
	assert.deepEqual(findings(result), [
		'type-mismatch PATCH /widgets/{} (http-api/widgets#widget-object).id: documented integer, schema string',
	]);
});

test('a type outside every anyOf branch fails', () => {
	const result = verify(mutate('| value | boolean | The value |', '| value | integer | The value |'));
	assert.deepEqual(findings(result), [
		'type-mismatch PATCH /widgets/{} (http-api/widgets#widget-object).value: documented integer, schema boolean | string',
	]);
});

test('a route-local response body table is type checked', () => {
	const result = verify(
		mutate('| total | integer | Every matching widget |', '| total | string | Every matching widget |'),
	);
	assert.deepEqual(findings(result), ['type-mismatch GET /widgets total: documented string, schema integer']);
});

test('a response body field the schema does not have fails', () => {
	const result = verify(
		mutate(
			'| total | integer | Every matching widget |\n',
			'| total | integer | Every matching widget |\n| count | integer | Stale |\n',
		),
	);
	assert.deepEqual(findings(result), ['response-extra GET /widgets count']);
});

test('every table under JSON body is checked, not only the first', () => {
	const result = verify(
		mutate(
			'| size? | integer | The new size |\n',
			'| size? | integer | The new size |\n\n#### Extra structure\n\n| Field | Type | Description |\n| --- | --- | --- |\n| colour? | string | Stale |\n',
		),
	);
	assert.deepEqual(findings(result), ['body-extra PATCH /widgets/{} colour']);
});

test('an optionality disagreement is fatal', () => {
	const result = verify(mutate('| name | string | The new name |', '| name? | string | The new name |'));
	assert.deepEqual(findings(result), [
		'optionality PATCH /widgets/{} name: documented optional, schema marks it required',
	]);
	assert.equal(result.optionalityAdvisories.length, 1);
});

test('an object member the schema does not declare needs the footnote', () => {
	const result = verify(
		mutate('<sup>1</sup> Emitted on every response although the response schema does not declare it\n', ''),
	);
	assert.deepEqual(findings(result), ['object-extra PATCH /widgets/{} (http-api/widgets#widget-object).shown']);
});

test('a schema member an object never has needs the declared note', () => {
	const result = verify(mutate('The response schema also declares `unused`, which a part never has.\n', ''));
	assert.deepEqual(findings(result), [
		'object-missing PATCH /widgets/{} (http-api/widgets#widget-object).part (http-api/widgets#part-object).unused',
	]);
});

test('a member missing from a linked response object fails', () => {
	const result = verify(mutate('| name | string | The name |\n', ''));
	assert.deepEqual(findings(result), ['object-missing PATCH /widgets/{} (http-api/widgets#widget-object).name']);
});

test('a route whose schema has a JSON body needs a JSON body section', () => {
	const result = verify(
		mutate('### JSON body\n\nThe body is a [gadget creation object](#gadget-creation-object).\n\n', ''),
	);
	assert.deepEqual(findings(result), ['body-missing POST /gadgets no JSON body section documents the request body']);
});

test('a route whose schema has query parameters needs a query parameter table', () => {
	const result = verify(
		mutate(
			'### Query parameters\n\n| Field | Type | Description |\n| --- | --- | --- |\n| limit? | integer | The page size |\n| owner_id? | snowflake | Restrict to one owner |\n\n',
			'',
		),
	);
	assert.deepEqual(findings(result), ['query-missing GET /widgets limit', 'query-missing GET /widgets owner_id']);
});

test('an object linked as the value of a map is compared with the value schema', () => {
	const result = verify(mutate('| text | string | The text |', '| title | string | The text |'));
	assert.deepEqual(findings(result), [
		'object-missing PATCH /widgets/{} (http-api/widgets#widget-object).labels (http-api/widgets#label-object).text',
		'object-extra PATCH /widgets/{} (http-api/widgets#label-object).title',
	]);
});

test('a body that is an array of a linked object is compared with the item schema', () => {
	const result = verify(mutate('| position? | integer | The new position |', '| rank? | integer | The new position |'));
	assert.deepEqual(findings(result), [
		'body-extra PUT /widgets/positions rank',
		'body-missing PUT /widgets/positions position',
	]);
});

test('an object documented as a keyed map is compared through its value link', () => {
	const result = verify(mutate('| name | string | The name |\n\n## Owners by ID object', '\n## Owners by ID object'));
	assert.deepEqual(findings(result), ['object-missing GET /owners (http-api/widgets#owners-by-id-object).name']);
});

test('an object linked where the schema declares no members fails', () => {
	const result = verify(
		mutate(
			'| total | integer | Every matching widget |',
			'| total | [part](#part-object) object | Every matching widget |',
		),
	);
	assert.deepEqual(findings(result), [
		'type-mismatch GET /widgets total: documented object, schema integer',
		'object-unresolved GET /widgets total: the schema declares no members for (http-api/widgets#part-object)',
	]);
});

test('a query table on a route with no query parameters fails unless the footnote says so', () => {
	const table =
		'### Query parameters\n\n| Field | Type | Description |\n| --- | --- | --- |\n| token<sup>1</sup> | string | The token |\n\n';
	const anchor =
		'### JSON body\n\n| Field | Type | Description |\n| --- | --- | --- |\n| name | string | The new name |';
	const undocumented = verify(mutate(anchor, `${table}${anchor}`));
	assert.deepEqual(findings(undocumented), ['query-extra PATCH /widgets/{} token']);
	const note = '<sup>1</sup> Required, although the request schema does not declare it\n\n';
	const footnoted = verify(mutate(anchor, `${table}${note}${anchor}`));
	assert.deepEqual(findings(footnoted), []);
});

test('a body or response table on a route whose schema has neither fails', () => {
	const result = verify(
		mutate(
			'| 204 | empty | The widgets were moved |',
			'| 204 | empty | The widgets were moved |\n\n### Response body\n\n| Field | Type | Description |\n| --- | --- | --- |\n| moved | integer | Stale |',
		),
	);
	assert.deepEqual(findings(result), ['response-extra PUT /widgets/positions moved']);
});

test('each object linked for a union documents the members of its own variant', () => {
	const result = verify(mutate('| kind | string | `square` |\n', ''));
	assert.deepEqual(findings(result), [
		'object-missing PATCH /widgets/{} (http-api/widgets#widget-object).shape (http-api/widgets#circle-object, http-api/widgets#square-object).kind (variant 2)',
	]);
});

test('a member repeated by a second object linked for a union is type checked', () => {
	const result = verify(mutate('| kind | string | `square` |', '| kind | integer | `square` |'));
	assert.deepEqual(findings(result), [
		'type-mismatch PATCH /widgets/{} (http-api/widgets#widget-object).shape (http-api/widgets#circle-object, http-api/widgets#square-object).kind: documented integer, schema string',
	]);
});

test('a member with a declared shape and no linked object section is counted', () => {
	const result = verify(mutate('| part | ?[part](#part-object) object | The part |', '| part | ?object | The part |'));
	assert.deepEqual(findings(result), []);
	assert.equal(result.counters.unlinkedObjects, 1);
});

test('a declared note naming a field the object schema does not have fails', () => {
	const result = verify(
		mutate(
			'The response schema also declares `unused`, which a part never has.',
			'The response schema also declares `unused` and `retired`, which a part never has.',
		),
	);
	assert.deepEqual(findings(result), [
		'object-stale-declared PATCH /widgets/{} (http-api/widgets#part-object).retired',
	]);
});

test('a declared note naming a field the response schema does not have fails', () => {
	const result = verify(
		mutate(
			'| total | integer | Every matching widget |\n',
			'| total | integer | Every matching widget |\n\nThe response schema also declares `cursor`.\n',
		),
	);
	assert.deepEqual(findings(result), ['response-stale-declared GET /widgets cursor']);
});

test('a not-declared footnote on a member the object schema declares fails', () => {
	const result = verify(mutate('| name | string | The name |', '| name<sup>1</sup> | string | The name |'));
	assert.deepEqual(findings(result), [
		'object-stale-undeclared PATCH /widgets/{} (http-api/widgets#widget-object).name',
	]);
});

test('a not-declared footnote on a declared body field or query parameter fails', () => {
	const note = '<sup>1</sup> Required, although the request schema does not declare it\n\n';
	const body = verify(
		mutate('| size? | integer | The new size |', `| size?<sup>1</sup> | integer | The new size |\n\n${note.trimEnd()}`),
	);
	assert.deepEqual(findings(body), ['body-stale-undeclared PATCH /widgets/{} size']);
	const query = verify(
		mutate(
			'| limit? | integer | The page size |\n| owner_id? | snowflake | Restrict to one owner |',
			`| limit?<sup>1</sup> | integer | The page size |\n| owner_id? | snowflake | Restrict to one owner |\n\n${note.trimEnd()}`,
		),
	);
	assert.deepEqual(findings(query), ['query-stale-undeclared GET /widgets limit']);
});
