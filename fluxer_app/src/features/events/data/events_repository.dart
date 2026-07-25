import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:fluxer_app/core/api/fluxer_client_provider.dart';
import 'package:fluxer_app/features/events/domain/event.dart';

/// Repository for calendar event CRUD and RSVP via the Fluxer REST API.
class EventsRepository {
  const EventsRepository(this._ref);

  final Ref _ref;

  /// Returns the base API client so routes are constructed relative to the
  /// current instance (self-hosting support).
  dynamic get _client => _ref.read(fluxerClientProvider);

  /// Lists all events in a calendar channel.
  Future<List<GuildEvent>> listEvents(String channelId) async {
    final response = await _client.get('/channels/$channelId/events');
    final List<dynamic> data = response.data as List<dynamic>;
    return data
        .cast<Map<String, Object?>>()
        .map(GuildEvent.fromJson)
        .toList();
  }

  /// Fetches a single event by ID.
  Future<GuildEvent> getEvent(String channelId, String eventId) async {
    final response =
        await _client.get('/channels/$channelId/events/$eventId');
    return GuildEvent.fromJson(
      Map<String, Object?>.from(response.data as Map),
    );
  }

  /// Creates a new event in a calendar channel.
  Future<GuildEvent> createEvent({
    required String channelId,
    required String name,
    required DateTime startsAt,
    required DateTime endsAt,
    String? description,
    String? locationChannelId,
    String? locationText,
    EventRepeatType repeatType = EventRepeatType.never,
    int repeatInterval = 1,
  }) async {
    final body = <String, Object?>{
      'name': name,
      'starts_at': startsAt.toUtc().toIso8601String(),
      'ends_at': endsAt.toUtc().toIso8601String(),
      if (description != null) 'description': description,
      if (locationChannelId != null) 'location_channel_id': locationChannelId,
      if (locationText != null) 'location_text': locationText,
      'repeat_type': repeatType.value,
      'repeat_interval': repeatInterval,
    };
    final response = await _client.post(
      '/channels/$channelId/events',
      data: body,
    );
    return GuildEvent.fromJson(
      Map<String, Object?>.from(response.data as Map),
    );
  }

  /// Updates an existing event.
  Future<GuildEvent> updateEvent({
    required String channelId,
    required String eventId,
    String? name,
    DateTime? startsAt,
    DateTime? endsAt,
    String? description,
    String? locationChannelId,
    String? locationText,
  }) async {
    final body = <String, Object?>{
      if (name != null) 'name': name,
      if (startsAt != null) 'starts_at': startsAt.toUtc().toIso8601String(),
      if (endsAt != null) 'ends_at': endsAt.toUtc().toIso8601String(),
      if (description != null) 'description': description,
      if (locationChannelId != null) 'location_channel_id': locationChannelId,
      if (locationText != null) 'location_text': locationText,
    };
    final response = await _client.patch(
      '/channels/$channelId/events/$eventId',
      data: body,
    );
    return GuildEvent.fromJson(
      Map<String, Object?>.from(response.data as Map),
    );
  }

  /// Deletes an event.
  Future<void> deleteEvent(String channelId, String eventId) async {
    await _client.delete('/channels/$channelId/events/$eventId');
  }

  /// Toggles the current user's RSVP status for an event.
  /// Returns the updated event with the new [isAttending] and [attendeeCount].
  Future<GuildEvent> toggleRsvp(String channelId, String eventId) async {
    final response = await _client.put(
      '/channels/$channelId/events/$eventId/rsvp',
    );
    return GuildEvent.fromJson(
      Map<String, Object?>.from(response.data as Map),
    );
  }

  /// Returns the list of attendees for an event.
  Future<List<EventAttendee>> listAttendees(
    String channelId,
    String eventId,
  ) async {
    final response = await _client
        .get('/channels/$channelId/events/$eventId/attendees');
    final List<dynamic> data = response.data as List<dynamic>;
    return data
        .cast<Map<String, Object?>>()
        .map(EventAttendee.fromJson)
        .toList();
  }

  /// Returns a CalDAV-compatible .ics export URL for an event.
  String exportIcsUrl(String channelId, String eventId) {
    final baseUrl = _client.options.baseUrl as String;
    return '$baseUrl/channels/$channelId/events/$eventId/export.ics';
  }
}

final eventsRepositoryProvider = Provider<EventsRepository>(
  (ref) => EventsRepository(ref),
);
