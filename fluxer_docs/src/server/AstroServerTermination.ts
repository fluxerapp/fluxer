// SPDX-License-Identifier: AGPL-3.0-or-later

export type AstroServerShutdown = () => Promise<void>;

interface AstroServerTerminationDependencies {
	readonly closeListener: () => Promise<void>;
	readonly serverName: string;
	readonly shutdown: AstroServerShutdown | null;
	readonly timeoutMs: number;
}

export class AstroServerTermination {
	private installed = false;
	private terminationPromise: Promise<void> | null = null;

	public constructor(private readonly dependencies: AstroServerTerminationDependencies) {}

	public install(): void {
		if (this.installed) {
			throw new Error(`${this.dependencies.serverName} termination handlers are already installed`);
		}
		this.installed = true;
		process.on('SIGINT', this.handleSIGINT);
		process.on('SIGTERM', this.handleSIGTERM);
	}

	private readonly handleSIGINT = (): void => {
		this.startTermination('SIGINT');
	};

	private readonly handleSIGTERM = (): void => {
		this.startTermination('SIGTERM');
	};

	private startTermination(signal: NodeJS.Signals): void {
		if (this.terminationPromise != null) {
			return;
		}
		this.terminationPromise = this.performTermination(signal);
	}

	private async performTermination(signal: NodeJS.Signals): Promise<void> {
		const watchdog = setTimeout(() => {
			console.error(
				`Forcing ${this.dependencies.serverName} termination after cleanup timeout`,
				`${signal} ${this.dependencies.timeoutMs.toString()}ms`,
			);
			process.exit(1);
		}, this.dependencies.timeoutMs);
		const failures = await this.releaseResources();
		clearTimeout(watchdog);
		if (failures.length === 0) {
			process.exit(0);
		}
		const message = `${this.dependencies.serverName} shutdown failed`;
		console.error(message, new AggregateError(failures, message));
		process.exit(1);
	}

	private async releaseResources(): Promise<Array<unknown>> {
		const failures: Array<unknown> = [];
		try {
			await this.dependencies.closeListener();
		} catch (cleanupFailure) {
			failures.push(cleanupFailure);
		}
		const shutdown = this.dependencies.shutdown;
		if (shutdown == null) {
			return failures;
		}
		try {
			await shutdown();
		} catch (cleanupFailure) {
			failures.push(cleanupFailure);
		}
		return failures;
	}
}
