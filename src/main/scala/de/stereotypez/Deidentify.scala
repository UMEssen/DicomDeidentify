package de.stereotypez

import java.time.{Duration, LocalDate, LocalTime}
import java.util.Date

import org.dcm4che3.data.{Attributes, Tag, VR}
import org.dcm4che3.util.{TagUtils, UIDUtils}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import ActionCode._

object Deidentify {
  def apply() = new Deidentify()
}

class Deidentify() {

  type Tag = Int
  type TimeShiftFunction = (LocalTime, Date) => Date
  type DateShiftFunction = (LocalDate, Date) => Date
  type DateShiftPAF = Date => Date
  type TimeShiftPAF = Date => Date
  type CleaningFunction = (Attributes, Tag, DateShiftPAF, TimeShiftPAF) => Boolean

  private var _keep: Seq[Tag] = Array.empty[Tag]
  def keep(tags: Tag*): Deidentify = {
    _keep = tags
    this
  }

  // handlers with side effects can directly modify attributes
  // if a handler returns true it will override default (Supp. 142) behavior
  private var _specialHandlers = ListBuffer.empty[CleaningFunction]
  def addSpecialHandler(handler: CleaningFunction): Deidentify = {
    _specialHandlers += handler
    this
  }

  // use PatientBirthDate to normalize all dates by default
  private var _referenceDate: Attributes => LocalDate = (att: Attributes) => {
    att.getDate(Tag.PatientBirthDate, new Date(0))
  }
  def withReferenceDate(referenceDate: Attributes => LocalDate): Deidentify = {
    _referenceDate = referenceDate
    this
  }

  // use StudyTime to normalize all times by default
  private var _referenceTime: Attributes => LocalTime = (att: Attributes) => {
    att.getDate(Tag.StudyTime, new Date(0)).toLocalTime
  }
  def withReferenceTime(referenceTime: Attributes => LocalTime): Deidentify = {
    _referenceTime = referenceTime
    this
  }

  // will be partially applied with refDate on execute()
  private var _dateShiftFunction: DateShiftFunction = (refDate, d) => {
    toLocalDateTime(d).plus(Duration.between(toLocalDateTime(refDate), toLocalDateTime(new Date(0))))
  }
  def withDateShiftFunction(dateShiftFunction: DateShiftFunction): Deidentify = {
    _dateShiftFunction = dateShiftFunction
    this
  }

  // will be partially applied with refDate on execute()
  private var _timeShiftFunction: TimeShiftFunction = (refTime, d) => {
    toLocalDateTime(d).plus(Duration.between(refTime, toLocalDateTime(new Date(3600 * 1000 * 12))))
  }
  def withTimeShiftFunction(timeShiftFunction: TimeShiftFunction): Deidentify = {
    _timeShiftFunction = timeShiftFunction
    this
  }

  private var _profileOptions: Seq[ProfileOption] = List(BasicProfile)
  def withOptions(profileOptions: ProfileOption*): Deidentify = {
    _profileOptions = profileOptions
    this
  }

  // default cleaning functions
  private var _xcleanFunction: CleaningFunction = (att, tag, dsf, tsf) => {
    att.remove(tag)
    true
  }
  private var _ccleanFunction: CleaningFunction = (att, tag, dsf, tsf) => {
    att.getVR(tag) match {
      case vr if VR.SQ == att.getVR(tag) =>
        att.remove(tag)
        att.ensureSequence(tag, 0)
      case vr if vr.isTemporalType => att.setDate(tag, vr, dsf(att.getDate(tag)))
      case vr if vr == VR.PN => att.setString(tag, vr, _dummyFunction(tag))
      case vr if vr.isStringType => att.setString(tag, vr, _dummyFunction(tag))
      case vr if vr.isIntType => att.setInt(tag, vr, 0)
      case vr if vr.isInlineBinary => att.setBytes(tag, vr, _dummyFunction(tag).getBytes)
    }
    true
  }
  private var _dcleanFunction: CleaningFunction = (att, tag, dsf, tsf) => {
    att.getVR(tag) match {
      case vr if VR.SQ == att.getVR(tag) =>
        att.remove(tag)
        att.ensureSequence(tag, 0)
      case vr if vr.isTemporalType => att.setDate(tag, vr, new Date(0))
      case vr if vr == VR.PN => att.setString(tag, vr, _dummyFunction(tag))
      case vr if vr.isStringType => att.setString(tag, vr, _dummyFunction(tag))
      case vr if vr.isIntType => att.setInt(tag, vr, 0)
      case vr if vr.isInlineBinary => att.setBytes(tag, vr, _dummyFunction(tag).getBytes)
    }
    true
  }
  private var _ucleanFunction: CleaningFunction = (att, tag, dsf, tsf) => {
    att.setString(tag, att.getVR(tag), UIDUtils.createNameBasedUID(att.getBytes(tag)))
    true
  }
  private var _kcleanFunction: CleaningFunction = (att, tag, dsf, tsf) => {
    if (VR.SQ == att.getVR(tag)) {
      att.remove(tag)
      att.ensureSequence(tag, 0)
      true
    }
    else false
  }

  private val _cleaningFunctions: mutable.Map[ActionCode, CleaningFunction] = mutable.Map(
    // "X": "remove"
    `X` -> _xcleanFunction,
    // "C": "clean, that is replace with values of similar meaning known not to contain identifying
    // information and consistent with the VR"
    `C` -> _ccleanFunction,
    // "D": "replace with a non-zero length value that may be a dummy value and consistent with the VR"
    `D` -> _dcleanFunction,
    // "Z": "replace with a zero length value, or a non-zero length value that may be a dummy value and consistent with the VR"
    `Z` -> _dcleanFunction,
    // "U": "replace with a non-zero length UID that is internally consistent within a set of Instances"
    `U` -> _ucleanFunction,
    // "K": "keep (unchanged for non-sequence attributes, cleaned for sequences)"
    `K` -> _kcleanFunction,
    // "X/D": "X unless D is required to maintain IOD conformance (Type 3 versus Type 1)"
    `X/D` -> _xcleanFunction,
    // "X/Z": "X unless Z is required to maintain IOD conformance (Type 3 versus Type 2)"
    `X/Z` -> _xcleanFunction,
    // "X/Z/D": "X unless Z or D is required to maintain IOD conformance (Type 3 versus Type 2 versus Type 1)"
    `X/Z/D` -> _xcleanFunction,
    // "X/Z/U*": "X unless Z or replacement of contained instance UIDs (U) is required to maintain
    // IOD conformance (Type 3 versus Type 2 versus Type 1 sequences containing UID references)"
    `X/Z/U*` -> _xcleanFunction,
    // "Z/D": "Z unless D is required to maintain IOD conformance (Type 2 versus Type 1)"
    `Z/D` -> _dcleanFunction
  )

  def withCleaningFunction(actionCode: ActionCode, cleaningFunction: CleaningFunction): Deidentify = {
    _cleaningFunctions.put(actionCode, cleaningFunction)
    this
  }

  private var _dummyFunction: Tag => String = (tag) => "dummy"
  def withDummies(default: String, lookup: Map[Tag, String]): Deidentify = {
    _dummyFunction = (tag: Tag) => lookup.getOrElse(tag, default)
    this
  }

  def execute(attributes: Attributes): Attributes = {

    //  bind reference date and time before any modifications and use PAFs downstream
    deidentify(
      attributes,
      _dateShiftFunction(_referenceDate(attributes),_),
      _timeShiftFunction(_referenceTime(attributes),_)
    )

    attributes
  }

  def execute(attributes: Seq[Attributes]): Seq[Attributes] = {
    attributes.map(this.execute _)
  }

  private def deidentify(att: Attributes, dsf: DateShiftPAF, tsf: TimeShiftPAF): Unit = {

    att.tags foreach {

      // skip user defined tags completely
      case tag if _keep.contains(tag) => ()

      // apply special handlers and skip if one returns true
      case tag if _specialHandlers.exists(_(att, tag, dsf, tsf)) => ()

      // recurse over sequences
      case tag if VR.SQ == att.getVR(tag) =>
        att.getSequence(tag) forEach { seqatt =>
          deidentify(seqatt, dsf, tsf)
        }

      // apply DICOM Supp. 142 logic
      case tag =>
        deidentifiers.find(_.matches(tag)) map { d =>
          // foldLeft through profiles
          val code: ActionCode = _profileOptions.foldLeft(Option.empty[ActionCode]) { (a, b) =>
            b.apply(d) match {
              case Some(code) => Some(code)
              case _ => a
            }
          } getOrElse {BasicProfile(d).get}

          // apply the respective cleaning function
          _cleaningFunctions(code)(att, tag, dsf, tsf)
        }
    }

  }
}

