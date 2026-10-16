// SPDX-License-Identifier: AGPL-3.0-or-later

import {type RuntimeConfigSnapshot, runtimeInstanceKey} from '@app/features/app/state/InstanceSnapshotStore';
import {makeAutoObservable, observableRef} from 'mobx';
import {createContext, type ReactNode, useContext} from 'react';

function requireAuthRuntimeInstanceKey(snapshot: RuntimeConfigSnapshot): string {
	const instanceKey = runtimeInstanceKey(snapshot);
	if (instanceKey == null) {
		throw new Error('Auth flow received a runtime snapshot without a valid instance key');
	}
	return instanceKey;
}

type AuthRuntimeTargetPolicy =
	| {
			readonly kind: 'instance-selection';
			readonly initialSnapshot: RuntimeConfigSnapshot | null;
	  }
	| {
			readonly kind: 'browser-document';
			readonly snapshot: RuntimeConfigSnapshot;
			readonly instanceKey: string;
	  };

export class AuthRuntimeTarget {
	snapshot: RuntimeConfigSnapshot | null;
	readonly initialSnapshot: RuntimeConfigSnapshot | null;
	private readonly policy: AuthRuntimeTargetPolicy;

	private constructor(policy: AuthRuntimeTargetPolicy) {
		const snapshot = policy.kind === 'instance-selection' ? policy.initialSnapshot : policy.snapshot;
		if (snapshot !== null) {
			requireAuthRuntimeInstanceKey(snapshot);
		}
		this.snapshot = snapshot;
		this.initialSnapshot = snapshot;
		this.policy = policy;
		makeAutoObservable<AuthRuntimeTarget, 'policy' | 'initialSnapshot'>(
			this,
			{snapshot: observableRef, initialSnapshot: false, policy: false},
			{autoBind: true},
		);
	}

	static forInstanceSelection(snapshot: RuntimeConfigSnapshot | null = null): AuthRuntimeTarget {
		return new AuthRuntimeTarget({kind: 'instance-selection', initialSnapshot: snapshot});
	}

	static forBrowserDocument(snapshot: RuntimeConfigSnapshot): AuthRuntimeTarget {
		return new AuthRuntimeTarget({
			kind: 'browser-document',
			snapshot,
			instanceKey: requireAuthRuntimeInstanceKey(snapshot),
		});
	}

	select(snapshot: RuntimeConfigSnapshot): void {
		const instanceKey = requireAuthRuntimeInstanceKey(snapshot);
		if (this.policy.kind === 'browser-document' && instanceKey !== this.policy.instanceKey) {
			throw new Error(
				`Browser authentication cannot target instance "${instanceKey}" from document instance "${this.policy.instanceKey}"`,
			);
		}
		this.snapshot = snapshot;
	}

	reset(): void {
		this.snapshot = this.initialSnapshot;
	}
}

const AuthRuntimeTargetContext = createContext<AuthRuntimeTarget | null>(null);

interface AuthRuntimeTargetProviderProps {
	readonly target: AuthRuntimeTarget;
	readonly children: ReactNode;
}

export function AuthRuntimeTargetProvider({target, children}: AuthRuntimeTargetProviderProps) {
	return <AuthRuntimeTargetContext.Provider value={target}>{children}</AuthRuntimeTargetContext.Provider>;
}

export function useAuthRuntimeTarget(): AuthRuntimeTarget {
	const target = useContext(AuthRuntimeTargetContext);
	if (target == null) {
		throw new Error('useAuthRuntimeTarget must be used within AuthRuntimeTargetProvider');
	}
	return target;
}
