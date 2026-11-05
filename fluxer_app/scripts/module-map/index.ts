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

interface ConcatenatedModuleLike {
	modules?: Set<{resource?: string | null}>;
	rootModule?: {resource?: string | null} | null;
}

type CompilationModule = Parameters<Compilation['chunkGraph']['getModuleId']>[0];

const getConcatenatedModules = (module: CompilationModule): ConcatenatedModuleLike | undefined => {
	const concatenated = module as CompilationModule & ConcatenatedModuleLike;

	return concatenated.modules?.size ? concatenated : undefined;
};

const collectModuleMetadata = (compilation: Compilation, root: string, includeDependencies: boolean) => {
	const modules: Record<string, ModuleEntry> = {};
	const packages: Record<string, string> = {};

	for (const module of compilation.modules) {
		const id = compilation.chunkGraph.getModuleId(module);
		if (id == null) continue;

		const resource = getModuleResource(module);
		if (!resource || !path.isAbsolute(resource)) continue;

		const moduleFilename = stripQuery(resource);
		const nodeModuleInfo = getNodeModuleInfo(root, moduleFilename);
		const packageName = nodeModuleInfo?.packageName;
		const packagePath = nodeModuleInfo?.packagePath;

		if (packageName ? !includeDependencies : !isInRoot(root, moduleFilename)) {
			continue;
		}

		const key = packageName ? `npm/${packagePath}` : normalizePath(path.relative(root, moduleFilename));

		const concatenated = getConcatenatedModules(module);

		if (concatenated) {
			const rootModule = concatenated.rootModule ?? [...concatenated.modules!][0];

			for (const child of concatenated.modules!) {
				if (!child.resource || child === rootModule) continue;

				const childResource = stripQuery(child.resource);
				const childRelative = path.relative(root, childResource);

				if (isInRoot(root, childResource)) {
					modules[normalizePath(childRelative)] = {
						concatenatedInto: id,
					};
				}
			}
		}

		let moduleKey = key;
		if (!packageName && key.startsWith('apps/')) {
			moduleKey = key.slice(5);
		}

		const exportMap = getModuleExportMap(compilation, module);

		modules[moduleKey] = {
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
