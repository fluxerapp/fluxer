import {EMPTY_SCOPE, StaticPathResolver, UNRESOLVED} from '@fluxer/openapi/src/extractors/StaticPathResolver';
import {Project} from 'ts-morph';
import {describe, expect, it} from 'vitest';

function resolveExpression(expression: string) {
	const project = new Project({useInMemoryFileSystem: true});
	const source = project.createSourceFile(
		'RouteConstants.ts',
		`const paths = ['/first', '/second']; const path = ${expression};`,
	);
	return new StaticPathResolver(project).resolve(
		source.getVariableDeclarationOrThrow('path').getInitializerOrThrow(),
		EMPTY_SCOPE,
	);
}

describe('static route expressions', () => {
	it.each([0, '0', 1, '1'])('resolves the exact array index %j', (index) => {
		expect(resolveExpression(`paths[${JSON.stringify(index)}]`)).toBe(Number(index) === 0 ? '/first' : '/second');
	});

	it.each([
		'1suffix',
		'1.5',
		'01',
		'1e0',
		' 1',
		'',
		'-0',
		-1,
		2,
	])('does not coerce array property %j into a route index', (index) => {
		expect(resolveExpression(`paths[${JSON.stringify(index)}]`)).toBe(UNRESOLVED);
	});
});
