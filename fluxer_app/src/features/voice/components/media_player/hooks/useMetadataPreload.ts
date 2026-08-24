// SPDX-License-Identifier: AGPL-3.0-or-later

import {useCallback, useState} from 'react';

export type MediaPreloadAttribute = 'none' | 'metadata';

export interface UseMetadataPreloadReturn {
	escalateToMetadata: () => void;
	sourceAttribute: string | undefined;
	preloadAttribute: MediaPreloadAttribute;
}

export function useMetadataPreload(src: string, hasStarted: boolean): UseMetadataPreloadReturn {
	const [wantsMetadata, setWantsMetadata] = useState(false);
	const escalateToMetadata = useCallback(() => {
		setWantsMetadata(true);
	}, []);
	return {
		escalateToMetadata,
		sourceAttribute: hasStarted || wantsMetadata ? src : undefined,
		preloadAttribute: wantsMetadata ? 'metadata' : 'none',
	};
}
