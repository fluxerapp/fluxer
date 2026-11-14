import path from 'node:path';
import type {Compilation} from '@rspack/core';

type CompilationModule = Parameters<Compilation['chunkGraph']['getModuleId']>[0];

const ASSET_FILE_REGEX = /\.(?:m?js|cjs|css)$/i;

export const normalizePath = (value: string): string => value.replace(/\\/g, '/');

export const stripQuery = (value: string): string => value.split('?')[0];

export const isInRoot = (root: string, file: string): boolean => {
	const relative = path.relative(root, file);

	return !relative.startsWith('..') && !path.isAbsolute(relative);
};

export const getNodeModuleInfo = (root: string, file: string) => {
	const relative = normalizePath(path.relative(root, file));
	const nodeModulesIndex = relative.lastIndexOf('node_modules/');

	if (nodeModulesIndex === -1) return undefined;

	const packagePath = relative.slice(nodeModulesIndex + 'node_modules/'.length);
	const segments = packagePath.split('/');
	const packageName = segments[0]?.startsWith('@')
		? `${segments[0]}/${segments[1] ?? ''}`.replace(/\/$/, '')
		: segments[0];

	return packageName ? {packageName, packagePath: normalizePath(packagePath)} : undefined;
};

export const getModuleFiles = (compilation: Compilation, module: CompilationModule): Array<string> => {
	const files = new Set<string>();

	for (const chunk of compilation.chunkGraph.getModuleChunksIterable(module)) {
		for (const file of chunk.files) {
			if (ASSET_FILE_REGEX.test(file)) {
				files.add(file);
			}
		}
	}

	return [...files];
};

export const getModuleResource = (module: CompilationModule): string | undefined => {
	if ('resource' in module && typeof module.resource === 'string') {
		return module.resource;
	}

	const {rootModule} = module as CompilationModule & {rootModule?: {resource?: string | null} | null};

	return typeof rootModule?.resource === 'string' ? rootModule.resource : undefined;
};

export const isAssetFile = (file: string): boolean => ASSET_FILE_REGEX.test(file);
