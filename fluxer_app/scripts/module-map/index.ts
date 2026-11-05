import crypto from 'node:crypto';
import path from 'node:path';
import {Compilation, type Compiler, sources} from '@rspack/core';

import {getModuleExportMap} from './exports.ts';
import {
	getModuleFiles,
	getModuleResource,
	getNodeModuleInfo,
	isAssetFile,
	isInRoot,
	normalizePath,
	stripQuery,
} from './modules.ts';

interface ModuleMapPluginOptions {
	root: string;
	build?: Record<string, unknown>;
	filename?: string;
	includeDependencies?: boolean;
}

interface ModuleEntry {
	id?: string | number;
	files?: Array<string>;
	concatenatedInto?: string | number;
	exportMap?: Record<string, string>;
}

interface ModuleMapManifest {
	build: Record<string, unknown>;
	moduleIds: string | false;
	chunkLoadingGlobal: string;
	modules: Record<string, ModuleEntry>;
	packages: Record<string, string>;
	assets: Record<string, string>;
}

interface ModuleWithResource {
	resource?: string | null;
}

interface ConcatenatedModuleLike {
	modules?: Array<ModuleWithResource> | Set<ModuleWithResource>;
	rootModule?: ModuleWithResource | null;
}

type CompilationModule = Parameters<Compilation['chunkGraph']['getModuleId']>[0];

const getConcatenatedModules = (module: CompilationModule): Array<ModuleWithResource> | undefined => {
	const {modules} = module as CompilationModule & ConcatenatedModuleLike;

	if (modules == null) return undefined;

	const list = Array.isArray(modules) ? modules : [...modules];

	return list.length ? list : undefined;
};

const getModuleKey = (root: string, filename: string): {key: string; packageName?: string} => {
	const nodeModuleInfo = getNodeModuleInfo(root, filename);

	if (nodeModuleInfo) {
		return {key: `npm/${nodeModuleInfo.packagePath}`, packageName: nodeModuleInfo.packageName};
	}

	const relativePath = normalizePath(path.relative(root, filename));

	return {key: relativePath.startsWith('apps/') ? relativePath.slice(5) : relativePath};
};

const collectModuleMetadata = (compilation: Compilation, root: string, includeDependencies: boolean) => {
	const modules: Record<string, ModuleEntry> = {};
	const packages: Record<string, string> = {};
	const concatenatedChildren = new Map<string, string | number>();
	const concatenatedRoots = new Set<string>();

	for (const module of compilation.modules) {
		const concatenated = getConcatenatedModules(module);
		if (!concatenated) continue;

		const id = compilation.chunkGraph.getModuleId(module);
		if (id == null) continue;

		const rootModule = (module as CompilationModule & ConcatenatedModuleLike).rootModule ?? concatenated[0];

		if (typeof rootModule?.resource === 'string') {
			concatenatedRoots.add(stripQuery(rootModule.resource));
		}

		for (const child of concatenated) {
			if (!child.resource || child === rootModule || !path.isAbsolute(child.resource)) continue;

			concatenatedChildren.set(stripQuery(child.resource), id);
		}
	}

	for (const module of compilation.modules) {
		const resource = getModuleResource(module);
		if (!resource || !path.isAbsolute(resource)) continue;

		const moduleFilename = stripQuery(resource);
		const {key, packageName} = getModuleKey(root, moduleFilename);

		if (packageName ? !includeDependencies : !isInRoot(root, moduleFilename)) {
			continue;
		}

		const id = compilation.chunkGraph.getModuleId(module);

		if (id == null) {
			if (concatenatedRoots.has(moduleFilename)) continue;

			const concatenatedInto = concatenatedChildren.get(moduleFilename);
			if (concatenatedInto === undefined) continue;

			modules[key] = {concatenatedInto};

			continue;
		}

		const exportMap = getModuleExportMap(compilation, module);

		modules[key] = {
			id,
			files: getModuleFiles(compilation, module),
			...(exportMap ? {exportMap} : {}),
		};

		if (packageName) {
			const isEntry = [...compilation.moduleGraph.getIncomingConnections(module)].some(
				(connection) => connection.dependency?.request === packageName,
			);

			if (isEntry) {
				packages[packageName] = key;
			}
		}
	}

	return {modules, packages};
};

const collectAssetHashes = (compilation: Compilation): Record<string, string> => {
	const assets: Record<string, string> = {};

	for (const asset of compilation.getAssets()) {
		if (!isAssetFile(asset.name)) continue;

		assets[asset.name] = crypto.createHash('sha256').update(asset.source.buffer()).digest('hex');
	}

	return assets;
};

const createManifest = (
	compilation: Compilation,
	build: Record<string, unknown>,
	root: string,
	includeDependencies: boolean,
): ModuleMapManifest => {
	const {modules, packages} = collectModuleMetadata(compilation, root, includeDependencies);

	return {
		build,
		moduleIds: compilation.options.optimization.moduleIds ?? false,
		chunkLoadingGlobal: compilation.outputOptions.chunkLoadingGlobal ?? '',
		modules,
		packages,
		assets: collectAssetHashes(compilation),
	};
};

export function moduleMapPlugin({
	root,
	build = {},
	filename = 'module-map.json',
	includeDependencies = true,
}: ModuleMapPluginOptions) {
	const resolvedRoot = path.resolve(root);

	return {
		apply(compiler: Compiler): void {
			compiler.hooks.thisCompilation.tap('ModuleMapPlugin', (compilation) => {
				compilation.hooks.processAssets.tap(
					{
						name: 'ModuleMapPlugin',
						stage: Compilation.PROCESS_ASSETS_STAGE_REPORT,
					},
					() => {
						const manifest = createManifest(compilation, build, resolvedRoot, includeDependencies);

						compilation.emitAsset(filename, new sources.RawSource(JSON.stringify(manifest)));
					},
				);
			});
		},
	};
}
