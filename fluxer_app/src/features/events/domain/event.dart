import 'package:json_annotation/json_annotation.dart';

part 'event.g.dart';

enum EventRepeatType {
  never('never'),
  daily('daily'),
  weekly('weekly'),
  monthly('monthly');

  const EventRepeatType(this.value);

  final String value;

  static EventRepeatType fromString(String value) {
    for (final type in values) {
      if (type.value == value) return type;
    }
    return EventRepeatType.never;
  }
}

@JsonSerializable()
class GuildEvent {
  const GuildEvent({
    required this.id,
    required this.channelId,
    required this.guildId,
    required this.creatorId,
    required this.name,
    required this.startsAt,
    required this.endsAt,
    this.description,
    this.locationChannelId,
    this.locationText,
    this.repeatType = EventRepeatType.never,
    this.repeatInterval = 1,
    this.attendeeCount = 0,
    this.isAttending = false,
    this.createdAt,
    this.updatedAt,
  });

  factory GuildEvent.fromJson(Map<String, Object?> json) =>
      _$GuildEventFromJson(json);

  final String id;

  @JsonKey(name: 'channel_id')
  final String channelId;

  @JsonKey(name: 'guild_id')
  final String guildId;

  @JsonKey(name: 'creator_id')
  final String creatorId;

  final String name;

  @JsonKey(name: 'starts_at')
  final DateTime startsAt;

  @JsonKey(name: 'ends_at')
  final DateTime endsAt;

  final String? description;

  @JsonKey(name: 'location_channel_id')
  final String? locationChannelId;

  @JsonKey(name: 'location_text')
  final String? locationText;

  @JsonKey(name: 'repeat_type')
  final EventRepeatType repeatType;

  @JsonKey(name: 'repeat_interval')
  final int repeatInterval;

  @JsonKey(name: 'attendee_count')
  final int attendeeCount;

  @JsonKey(name: 'is_attending')
  final bool isAttending;

  @JsonKey(name: 'created_at')
  final DateTime? createdAt;

  @JsonKey(name: 'updated_at')
  final DateTime? updatedAt;

  bool get isOngoing {
    final now = DateTime.now().toUtc();
    return now.isAfter(startsAt.toUtc()) && now.isBefore(endsAt.toUtc());
  }

  bool get hasEnded => DateTime.now().toUtc().isAfter(endsAt.toUtc());

  GuildEvent copyWith({
    String? id,
    String? channelId,
    String? guildId,
    String? creatorId,
    String? name,
    DateTime? startsAt,
    DateTime? endsAt,
    String? description,
    String? locationChannelId,
    String? locationText,
    EventRepeatType? repeatType,
    int? repeatInterval,
    int? attendeeCount,
    bool? isAttending,
    DateTime? createdAt,
    DateTime? updatedAt,
  }) {
    return GuildEvent(
      id: id ?? this.id,
      channelId: channelId ?? this.channelId,
      guildId: guildId ?? this.guildId,
      creatorId: creatorId ?? this.creatorId,
      name: name ?? this.name,
      startsAt: startsAt ?? this.startsAt,
      endsAt: endsAt ?? this.endsAt,
      description: description ?? this.description,
      locationChannelId: locationChannelId ?? this.locationChannelId,
      locationText: locationText ?? this.locationText,
      repeatType: repeatType ?? this.repeatType,
      repeatInterval: repeatInterval ?? this.repeatInterval,
      attendeeCount: attendeeCount ?? this.attendeeCount,
      isAttending: isAttending ?? this.isAttending,
      createdAt: createdAt ?? this.createdAt,
      updatedAt: updatedAt ?? this.updatedAt,
    );
  }

  Map<String, Object?> toJson() => _$GuildEventToJson(this);
}

@JsonSerializable()
class EventAttendee {
  const EventAttendee({
    required this.userId,
    required this.rsvpAt,
    this.username,
    this.avatarHash,
    this.displayName,
  });

  factory EventAttendee.fromJson(Map<String, Object?> json) =>
      _$EventAttendeeFromJson(json);

  @JsonKey(name: 'user_id')
  final String userId;

  @JsonKey(name: 'rsvp_at')
  final DateTime rsvpAt;

  final String? username;

  @JsonKey(name: 'avatar_hash')
  final String? avatarHash;

  @JsonKey(name: 'display_name')
  final String? displayName;

  Map<String, Object?> toJson() => _$EventAttendeeToJson(this);
}
