// SPDX-License-Identifier: AGPL-3.0-or-later

const MODULE_INSTALL_OPERATIONS_MAX = 16;

interface ModuleInstallOperation<T> {
	readonly identity: string;
	readonly moduleName: string;
	readonly sha256: string;
	readonly run: () => Promise<T>;
	readonly promise: Promise<T>;
	readonly resolve: (value: T) => void;
	readonly reject: (error: unknown) => void;
}

class ModuleInstallCoordinatorCapacityError extends Error {
	public constructor() {
		super(`At most ${MODULE_INSTALL_OPERATIONS_MAX} distinct module installs may be pending`);
		this.name = 'ModuleInstallCoordinatorCapacityError';
	}
}

export class ModuleInstallCoordinator<T> {
	private readonly operations = new Map<string, ModuleInstallOperation<T>>();
	private readonly pending: Array<ModuleInstallOperation<T>> = [];
	private readonly activeModuleNames = new Set<string>();
	private readonly activeSha256Digests = new Set<string>();

	public run(moduleName: string, sha256: string, operation: () => Promise<T>): Promise<T> {
		const identity = `${moduleName}\0${sha256}`;
		const existingOperation = this.operations.get(identity);
		if (existingOperation != null) {
			return existingOperation.promise;
		}
		if (this.operations.size >= MODULE_INSTALL_OPERATIONS_MAX) {
			throw new ModuleInstallCoordinatorCapacityError();
		}

		let resolveOperation!: (value: T) => void;
		let rejectOperation!: (error: unknown) => void;
		const promise = new Promise<T>((resolve, reject) => {
			resolveOperation = resolve;
			rejectOperation = reject;
		});
		const queued: ModuleInstallOperation<T> = {
			identity,
			moduleName,
			sha256,
			run: operation,
			promise,
			resolve: resolveOperation,
			reject: rejectOperation,
		};
		this.operations.set(identity, queued);
		this.pending.push(queued);
		this.startAvailable();
		return promise;
	}

	private startAvailable(): void {
		for (let index = 0; index < this.pending.length; ) {
			const operation = this.pending[index];
			if (this.activeModuleNames.has(operation.moduleName) || this.activeSha256Digests.has(operation.sha256)) {
				index += 1;
				continue;
			}
			this.pending.splice(index, 1);
			this.activeModuleNames.add(operation.moduleName);
			this.activeSha256Digests.add(operation.sha256);
			void Promise.resolve()
				.then(operation.run)
				.then(
					(value) => {
						this.complete(operation);
						operation.resolve(value);
					},
					(error: unknown) => {
						this.complete(operation);
						operation.reject(error);
					},
				);
		}
	}

	private complete(operation: ModuleInstallOperation<T>): void {
		this.activeModuleNames.delete(operation.moduleName);
		this.activeSha256Digests.delete(operation.sha256);
		this.operations.delete(operation.identity);
		this.startAvailable();
	}
}
