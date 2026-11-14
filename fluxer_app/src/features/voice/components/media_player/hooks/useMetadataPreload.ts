// SPDX-License-Identifier: AGPL-3.0-or-later

import {useApiAttachmentPlaybackSrc} from '@app/features/messaging/hooks/useApiAttachmentSources';
import {useCallback, useState} from 'react';

type MediaPreloadAttribute = 'none' | 'metadata';

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
	const sourceAttribute = useApiAttachmentPlaybackSrc(hasStarted || wantsMetadata ? src : undefined);
	return {
		escalateToMetadata,
		sourceAttribute,
		preloadAttribute: wantsMetadata ? 'metadata' : 'none',
	};
}
