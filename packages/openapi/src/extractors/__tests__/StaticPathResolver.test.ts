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

describe('imported route constants', () => {
	function resolveImportedPath(specifier: string) {
		const project = new Project({useInMemoryFileSystem: true});
		project
			.getFileSystem()
			.writeFileSync('/api/tsconfig.json', JSON.stringify({compilerOptions: {paths: {'@app/*': ['./src/*']}}}));
		project.createSourceFile('/api/src/constants/Routes.ts', "export const USERS_ROUTE = '/users';");
		const controller = project.createSourceFile(
			'/api/src/users/UserController.ts',
			`import {USERS_ROUTE} from '${specifier}'; const path = USERS_ROUTE;`,
		);
		return new StaticPathResolver(project).resolve(
			controller.getVariableDeclarationOrThrow('path').getInitializerOrThrow(),
			EMPTY_SCOPE,
		);
	}

	it.each(['../constants/Routes', '@app/constants/Routes'])('follows %s to the exported constant', (specifier) => {
		expect(resolveImportedPath(specifier)).toBe('/users');
	});

	it('does not follow a bare package import', () => {
		expect(resolveImportedPath('@fluxer/constants/src/Routes')).toBe(UNRESOLVED);
	});
});
