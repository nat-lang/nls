
export type Interpretation = {

}
export interface IInterpretationService {
  interpret: (filepath: string) => Interpretation;
}
