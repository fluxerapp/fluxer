// SPDX-License-Identifier: AGPL-3.0-or-later

import {useEffect, useState} from 'react';

interface AuthSingleUseRequest {
	isCurrent: () => boolean;
}

type AuthSingleUseRequestWork = (request: AuthSingleUseRequest) => void | Promise<void>;

class AuthSingleUseRequestOwner {
	private readonly claimedKeys = new Set<string>();
	private mounted = false;
	private activeKey: string | null = null;

	public attach(): void {
		this.mounted = true;
	}

	public detach(): void {
		this.mounted = false;
	}

	public run(key: string, work: AuthSingleUseRequestWork): void {
		this.activeKey = key;
		if (this.claimedKeys.has(key)) {
			return;
		}
		this.claimedKeys.add(key);
		void work({isCurrent: () => this.mounted && this.activeKey === key});
	}
}

export function useAuthSingleUseRequest(key: string, work: AuthSingleUseRequestWork): void {
	const [owner] = useState(() => new AuthSingleUseRequestOwner());
	useEffect(() => {
		owner.attach();
		owner.run(key, work);
		return () => owner.detach();
	}, [key, owner]);
}
